{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal representation of Feldspar programs

module Feldspar.Representation where



import Data.Complex
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
newtype Ref a = Ref { unRef :: Struct PrimType' Imp.Ref a }
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

-- | Mutable array
newtype Arr a = Arr { unArr :: Struct PrimType' (Imp.Arr Index) a }
  -- An array of tuples is represented as a struct of smaller arrays. See
  -- comment to `Ref`.

-- | Immutable array
newtype IArr a = IArr { unIArr :: Struct PrimType' (Imp.IArr Index) a }



--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

-- | For loop
data ForLoop sig
  where
    ForLoop :: Type st =>
        ForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

instance Eval ForLoop
  where
    evalSym ForLoop = \len init body ->
        foldl (flip body) init $ genericTake len [0..]

instance EvalEnv ForLoop env

instance StringTree ForLoop

-- | Interaction with the IO layer
data IOSym sig
  where
    -- Turn a program into a pure value
    UnsafePerform :: Comp (Data a) -> IOSym (Full a)
    -- Identity function with a side effect
    UnsafePerformWith :: Comp () -> IOSym (a :-> Full a)
  -- The reason for having `UnsafeArrIx` instead of doing the same thing using
  -- `UnsafePerform` is that `UnsafeArrIx` can be compared for equality, which
  -- may help some optimizations.

instance Render IOSym
  where
    renderSym (UnsafePerform _)     = "UnsafePerform ..."
    renderSym (UnsafePerformWith _) = "UnsafePerformWith ..."

instance StringTree IOSym

instance Eval IOSym
  where
    evalSym s = error $ "eval: cannot evaluate unsafe operation " ++ renderSym s

instance EvalEnv IOSym env

-- | 'equal' always returns 'False'
instance Equality IOSym
  where
    equal _ _ = False

type FeldConstructs
    =   BindingT
    :+: Let
    :+: Tuple
    :+: Primitive
    :+: ForLoop
    :+: IOSym

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

-- | Evaluate a closed expression
eval :: (Syntactic a, Domain a ~ FeldDomain) => a -> Internal a
eval = evalClosed . desugar
  -- Note that a `Syntax` constraint would rule out evaluating functions

instance Imp.FreeExp Data
  where
    type FreePred Data = PrimType'
    constExp = sugarSymFeldPrim . Lit
    varExp   = sugarSymFeldPrim . FreeVar

instance Imp.EvalExp Data
  where
    evalExp = eval



--------------------------------------------------------------------------------
-- * Monadic computations
--------------------------------------------------------------------------------

type CompCMD
  =               Imp.RefCMD
  Operational.:+: Imp.ArrCMD
  Operational.:+: Imp.ControlCMD

-- | Monad for computational effects: mutable data structures and control flow
newtype Comp a = Comp
    { unComp ::
        Operational.Program CompCMD (Operational.Param2 Data PrimType') a
    }
  deriving (Functor, Applicative, Monad)



--------------------------------------------------------------------------------
-- Template Haskell instances
--------------------------------------------------------------------------------

deriveSymbol    ''ForLoop
deriveRender id ''ForLoop
deriveEquality  ''ForLoop

deriveSymbol ''IOSym

