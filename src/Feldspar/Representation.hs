{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

-- | Internal representation of Feldspar programs

module Feldspar.Representation where



import Control.Monad.Reader
import Data.Complex
import Data.Hash (hashInt)
import Data.Int
import Data.List (genericTake)
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Word

import Data.Constraint (Dict (..))

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import qualified Control.Monad.Operational.Higher as Operational

import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative.CMD as Imp
import qualified Language.Embedded.Backend.C.Expression as Imp

import Data.Inhabited
import Data.Selection
import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()



--------------------------------------------------------------------------------
-- * Object-language types
--------------------------------------------------------------------------------

-- | Representation of all supported types
type TypeRep = Struct PrimType' PrimTypeRep

-- | Supported types
class (Eq a, Show a, Typeable a, Inhabited a) => Type a
  where
    -- | Reify a type
    typeRep :: TypeRep a

instance Type Bool             where typeRep = Single BoolT
instance Type Int8             where typeRep = Single Int8T
instance Type Int16            where typeRep = Single Int16T
instance Type Int32            where typeRep = Single Int32T
instance Type Int64            where typeRep = Single Int64T
instance Type Word8            where typeRep = Single Word8T
instance Type Word16           where typeRep = Single Word16T
instance Type Word32           where typeRep = Single Word32T
instance Type Word64           where typeRep = Single Word64T
instance Type Float            where typeRep = Single FloatT
instance Type Double           where typeRep = Single DoubleT
instance Type (Complex Float)  where typeRep = Single ComplexFloatT
instance Type (Complex Double) where typeRep = Single ComplexDoubleT
instance (Type a, Type b) => Type (a,b) where typeRep = Two typeRep typeRep

-- | Alias for the conjunction of 'PrimType'' and 'Type'
class    (PrimType' a, Type a) => PrimType a
instance (PrimType' a, Type a) => PrimType a

instance Imp.CompTypeClass PrimType
  where
    compType _ = Imp.compType (Proxy :: Proxy PrimType')
    compLit _  = Imp.compLit (Proxy :: Proxy PrimType')
  -- This instance is used by <https://github.com/kmate/raw-feldspar-mcs>

-- | Convert any 'Struct' with a 'PrimType' constraint to a 'TypeRep'
toTypeRep :: Struct PrimType' c a -> TypeRep a
toTypeRep = mapStruct (const primTypeRep)

-- | Check whether two type representations are equal
typeEq :: TypeRep a -> TypeRep b -> Maybe (Dict (a ~ b))
typeEq (Single t) (Single u) = primTypeEq t u
typeEq (Two t1 t2) (Two u1 u2) = do
    Dict <- typeEq t1 u1
    Dict <- typeEq t2 u2
    return Dict
typeEq _ _ = Nothing

-- | Reflect a 'TypeRep' to a 'Typeable' constraint
witTypeable :: TypeRep a -> Dict (Typeable a)
witTypeable (Single t)
    | Dict <- witPrimType t
    = Dict
witTypeable (Two ta tb)
    | Dict <- witTypeable ta
    , Dict <- witTypeable tb
    = Dict

-- | Representation of supported value types + N-ary functions over such types
data TypeRepFun a
  where
    ValT :: TypeRep a -> TypeRepFun a
    FunT :: TypeRep a -> TypeRepFun b -> TypeRepFun (a -> b)
  -- Another option would have been to make `FunT` a constructor in `TypeRep`.
  -- That would have got rid of the extra layer at the expense of less accurate
  -- types (functions would be allowed in pairs, etc.). The current design was
  -- chosen in order to be able to reuse `Struct` instead of making `TypeRep` a
  -- new data type.

-- | Check whether two type representations are equal
typeEqFun :: TypeRepFun a -> TypeRepFun b -> Maybe (Dict (a ~ b))
typeEqFun (ValT t)     (ValT u)     = typeEq t u
typeEqFun (FunT ta tb) (FunT ua ub) = do
    Dict <- typeEq ta ua
    Dict <- typeEqFun tb ub
    return Dict
typeEqFun _ _ = Nothing

-- | Mutable variable
newtype Ref a = Ref { unRef :: Struct PrimType' Imp.Ref (Internal a) }
  -- A reference to a tuple is a struct of smaller references. This means that
  -- creating a reference to a tuple will generate several calls to generate new
  -- references. This must be done already in the front end, which means that
  -- the work in the back end becomes simpler.
  --
  -- Another option would be to allow a single reference to refer to a tuple,
  -- and then turn that into smaller references in the back end. However, this
  -- would complicate the back end for no obvious reason. (One way to do it
  -- would be to run the back end in a store that maps each front end reference
  -- to a struct of small references. Among other things, that would require
  -- dynamic typing.)

-- | Reference specialized to 'Data' elements
type DRef a = Ref (Data a)

instance Eq (Ref a)
  where
    Ref a == Ref b = and $ zipListStruct (==) a b

-- | Mutable array
data Arr a = Arr
    { arrOffset :: Data Index
    , arrLength :: Data Length
    , unArr     :: Struct PrimType' (Imp.Arr Index) (Internal a)
    }
  -- `arrOffset` gives the offset into the internal arrays. The user should not
  -- be able to access the offset or the internal arrays.
  --
  -- `arrLength` gives the maximum index+1 according to the view provided by
  -- `arrIx`. Thus, the maximum index in the internal arrays is
  -- `arrLength + arrOffset - 1`.
  -- `arrLength` should not be exported directly to prevent the user from using
  -- record syntax to change the length of an array. But an equivalent function
  -- can be exported.

  -- An array of tuples is represented as a struct of smaller arrays. See
  -- comment to `Ref`.

-- | Array specialized to 'Data' elements
type DArr a = Arr (Data a)

-- | '==' checks if two 'Arr' use the same physical array. The length and offset
-- are ignored.
instance Eq (Arr a)
  where
    Arr _ _ arr1 == Arr _ _ arr2 = and (zipListStruct (==) arr1 arr2)

-- | Immutable array
data IArr a = IArr
    { iarrOffset :: Data Index
    , iarrLength :: Data Length
    , unIArr     :: Struct PrimType' (Imp.IArr Index) (Internal a)
    }

-- | Immutable array specialized to 'Data' elements
type DIArr a = IArr (Data a)

-- | Check if an 'Arr' and and 'IArr' use the same physical array. The length
-- and offset are ignored. This operation may give false negatives, but never
-- false positives. Whether or not false negatives occur may also depend on the
-- interpretation of the program.
--
-- Due to this unreliability, the function should only be used to affect the
-- non-functional properties of a program (e.g. to avoid unnecessary array
-- copying).
unsafeEqArrIArr :: Arr a -> IArr a -> Bool
unsafeEqArrIArr (Arr _ _ arr1) (IArr _ _ arr2) =
    and (zipListStruct sameId arr1 arr2)
  where
    sameId :: Imp.Arr Index a -> Imp.IArr Index a -> Bool
    sameId (Imp.ArrComp a) (Imp.IArrComp i) = a==i
    sameId _ _ = False



--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

-- | Assertion labels
data AssertionLabel
    = InternalAssertion
        -- ^ Internal assertion to guarantee meaningful results
    | LibraryAssertion String
        -- ^ Assertion related to a specific library
    | UserAssertion String
        -- ^ Assertion in user code. The default label for user assertions is
        --   @`UserAssertion` ""@
  deriving (Eq, Show)

-- | A selection that includes all labels defined as 'UserAssertion'
onlyUserAssertions :: Selection AssertionLabel
onlyUserAssertions = selectBy $ \l -> case l of
    UserAssertion _ -> True
    _ -> False

data ExtraPrimitive sig
  where
    -- Integer division assuming `divBalanced x y * y == x` (i.e. no remainder).
    -- The purpose of the assumption is to use it in simplifications.
    DivBalanced :: (Integral a, PrimType' a) =>
        ExtraPrimitive (a :-> a :-> Full a)

    -- Guard a value by an assertion
    GuardVal ::
        AssertionLabel -> String -> ExtraPrimitive (Bool :-> a :-> Full a)

instance Eval ExtraPrimitive
  where
    evalSym DivBalanced = \a b -> if a `mod` b /= 0
      then error $ unwords ["divBalanced", show a, show b, "is not balanced"]
      else div a b
    evalSym (GuardVal _ msg) = \cond a ->
        if cond then a else error $ "Feldspar assertion failure: " ++ msg

instance Equality ExtraPrimitive
  where
    equal DivBalanced    DivBalanced    = True
    equal (GuardVal _ _) (GuardVal _ _) = True
    equal _ _ = False

    hash DivBalanced    = hashInt 1
    hash (GuardVal _ _) = hashInt 2

-- | For loop
data ForLoop sig
  where
    ForLoop :: Type st =>
        ForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

instance Eval ForLoop
  where
    evalSym ForLoop = \len init body ->
        foldl (flip body) init $ genericTake len [0..]

-- | Interaction with the IO layer
data Unsafe sig
  where
    -- Turn a program into a pure value
    UnsafePerform :: Comp (Data a) -> Unsafe (Full a)

instance Render Unsafe
  where
    renderSym (UnsafePerform _) = "UnsafePerform ..."

instance Eval Unsafe
  where
    evalSym s = error $ "eval: cannot evaluate unsafe operation " ++ renderSym s

-- | 'equal' always returns 'False'
instance Equality Unsafe
  where
    equal _ _ = False

type FeldConstructs
    =   BindingT
    :+: Let
    :+: Tuple
    :+: Primitive
    :+: ExtraPrimitive
    :+: ForLoop
    :+: Unsafe

type FeldDomain = FeldConstructs :&: TypeRepFun

newtype Data a = Data { unData :: ASTF FeldDomain a }

-- | Declaring 'Data' as syntactic sugar
instance Syntactic (Data a)
  where
    type Domain (Data a)   = FeldDomain
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

instance Syntactic (Struct PrimType' Data a)
    -- Note that this instance places no constraints on `a`. This is crucial in
    -- the way it is used in the rest of the code. It would be possible to
    -- define `desugar` and `sugar` in terms of the instance for pairs; however,
    -- that would require constraining `a`.
  where
    type Domain   (Struct PrimType' Data a) = FeldDomain
    type Internal (Struct PrimType' Data a) = a

    desugar (Single a) = unData a
    desugar (Two a b)  = sugarSymDecor (ValT $ Two ta tb) Pair a' b'
      where
        a' = desugar a
        b' = desugar b
        ValT ta = getDecor a'
        ValT tb = getDecor b'

    sugar a = case getDecor a of
        ValT (Single _)  -> Single $ Data a
        ValT (Two ta tb) ->
            Two (sugarSymDecor (ValT ta) Fst a) (sugarSymDecor (ValT tb) Snd a)

-- | Specialization of the 'Syntactic' class for the Feldspar domain
class    (Syntactic a, Domain a ~ FeldDomain, Type (Internal a)) => Syntax a
instance (Syntactic a, Domain a ~ FeldDomain, Type (Internal a)) => Syntax a

-- | Make a smart constructor for a symbol
sugarSymFeld
    :: ( Signature sig
       , fi         ~ SmartFun FeldDomain sig
       , sig        ~ SmartSig fi
       , FeldDomain ~ SmartSym fi
       , SyntacticN f fi
       , sub :<: FeldConstructs
       , Type (DenResult sig)
       )
    => sub sig -> f
sugarSymFeld = sugarSymDecor $ ValT typeRep

-- | Make a smart constructor for a symbol
sugarSymFeldPrim
    :: ( Signature sig
       , fi         ~ SmartFun FeldDomain sig
       , sig        ~ SmartSig fi
       , FeldDomain ~ SmartSym fi
       , SyntacticN f fi
       , sub :<: FeldConstructs
       , PrimType' (DenResult sig)
       )
    => sub sig -> f
sugarSymFeldPrim = sugarSymDecor $ ValT $ Single primTypeRep

instance Imp.FreeExp Data
  where
    type FreePred Data = PrimType'
    constExp = sugarSymFeldPrim . Lit
    varExp   = sugarSymFeldPrim . FreeVar



--------------------------------------------------------------------------------
-- * Monadic computations
--------------------------------------------------------------------------------

data AssertCMD fs a
  where
    Assert
        :: AssertionLabel
        -> exp Bool
        -> String
        -> AssertCMD (Operational.Param3 prog exp pred) ()

instance Operational.HFunctor AssertCMD
  where
    hfmap _ (Assert c cond msg) = Assert c cond msg

instance Operational.HBifunctor AssertCMD
  where
    hbimap _ g (Assert c cond msg) = Assert c (g cond) msg

instance Operational.InterpBi AssertCMD IO (Operational.Param1 PrimType')
  where
    interpBi (Assert _ cond msg) = do
        cond' <- cond
        unless cond' $ error $ "Assertion failed: " ++ msg

type CompCMD
  =               Imp.RefCMD
  Operational.:+: Imp.ArrCMD
  Operational.:+: Imp.ControlCMD
  Operational.:+: AssertCMD

-- | Monad for computational effects: mutable data structures and control flow
newtype Comp a = Comp
    { unComp ::
        Operational.Program CompCMD (Operational.Param2 Data PrimType') a
    }
  deriving (Functor, Applicative, Monad)



--------------------------------------------------------------------------------
-- Template Haskell instances
--------------------------------------------------------------------------------

deriveSymbol    ''ExtraPrimitive
deriveRender id ''ExtraPrimitive

deriveSymbol    ''ForLoop
deriveRender id ''ForLoop
deriveEquality  ''ForLoop

deriveSymbol ''Unsafe



--------------------------------------------------------------------------------
-- * Interpretation
--------------------------------------------------------------------------------

-- The stuff below depends on the TH-generated instances. From GHC 9, it seems
-- that generated instances are only in scope after the splice. The alternative
-- to move the splices up into the code is not possible (at least not easily)
-- because of the mutual dependencies between the different types. (Definitions
-- that occur after a splice are not in scope before the splice.)

instance EvalEnv ExtraPrimitive env
instance StringTree ExtraPrimitive

instance EvalEnv ForLoop env
instance StringTree ForLoop

instance EvalEnv Unsafe env
instance StringTree Unsafe

-- | Evaluate a closed expression
eval :: (Syntactic a, Domain a ~ FeldDomain) => a -> Internal a
eval = evalClosed . desugar
  -- Note that a `Syntax` constraint would rule out evaluating functions

instance Imp.EvalExp Data
  where
    evalExp = eval
