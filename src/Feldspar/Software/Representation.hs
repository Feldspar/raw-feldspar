{-# language TypeOperators #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

{-# language InstanceSigs #-}
{-# language Rank2Types #-}
{-# language ConstraintKinds #-}

{-# language TemplateHaskell #-}

module Feldspar.Software.Representation where

import Control.Monad.Trans

import Data.Complex
import Data.Constraint (Dict(..))
import Data.Hash (hashInt)
import Data.Int
import Data.Word
import Data.List (genericTake)
import Data.Typeable (Typeable)
import Data.Proxy

-- syntactic.
import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH
import qualified Language.Syntactic as Syntactic ((:+:))

-- operational-higher.
import qualified Control.Monad.Operational.Higher as Operational

-- imperative-edsl.
import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative as Imp hiding ((:+:))
import qualified Language.Embedded.Imperative.CMD as Imp
import qualified Language.Embedded.Concurrent as Imp

import Data.TypedStruct
import Data.Inhabited

import Feldspar.Representation
import Feldspar.Software.Primitive hiding (Exp)

--------------------------------------------------------------------------------
-- * Software language.
--------------------------------------------------------------------------------

-- | Software only instructions.
type SoftwareCMD
              =   Imp.ControlCMD
  Operational.:+: Imp.PtrCMD
  Operational.:+: Imp.ThreadCMD
  Operational.:+: Imp.ChanCMD
  Operational.:+: Imp.FileCMD
  Operational.:+: Imp.C_CMD

-- | Monad for running software programs.
newtype Software a = Software
  { unSoftware ::
      Operational.ProgramT
        SoftwareCMD
        (Operational.Param2 Exp SoftwarePrimType)
        (Operational.Program
           CompCMD
           (Operational.Param2 Exp SoftwarePrimType))
        a
  } deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------

-- software programs support computational effects.
instance MonadComp Software
  where
    type Expr Software = Exp
    type Pred Software = SoftwarePrimType
    type TRep Software = SoftwarePrimTypeRep
    
    liftComp = Software . lift

--------------------------------------------------------------------------------

-- | Mutable variable.
newtype Ref a = Ref { unRef :: Struct SoftwarePrimType Imp.Ref (Internal a) }
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

-- | Reference specialized to 'Exp' elements.
type SRef a = Ref (Exp a)

instance Eq (Ref a)
  where
    Ref a == Ref b = and $ zipListStruct (==) a b

--------------------------------------------------------------------------------

-- | Mutable array.
data Arr a = Arr
    { arrOffset :: Exp Index
    , arrLength :: Exp Length
    , unArr     :: Struct SoftwarePrimType (Imp.Arr Index) (Internal a)
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
  --
  -- An array of tuples is represented as a struct of smaller arrays. See
  -- comment to `Ref`.

-- | Array specialized to 'Exp' elements
type SArr a = Arr (Exp a)

-- | '==' checks if two 'Arr' use the same physical array. The length and offset
-- are ignored.
instance Eq (Arr a)
  where
    Arr _ _ arr1 == Arr _ _ arr2 = and (zipListStruct (==) arr1 arr2)

--------------------------------------------------------------------------------

-- | Immutable array.
data IArr a = IArr
    { iarrOffset :: Exp Index
    , iarrLength :: Exp Length
    , unIArr     :: Struct SoftwarePrimType (Imp.IArr Index) (Internal a)
    }

-- | Immutable array specialized to 'Exp' elements
type SIArr a = IArr (Exp a)

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
-- ** Software types.
--------------------------------------------------------------------------------

-- any primitive software type is aslo a valid software type.
instance SoftwarePrimType a => Type SoftwarePrimType SoftwarePrimTypeRep a
  where
    typeRep = Single softwareTypeRep

-- short-hand for the conjunction of 'Type' for our software language and 'SoftwarePrimType'.
class    (Type SoftwarePrimType SoftwarePrimTypeRep a, SoftwarePrimType a) => SoftwareType a
instance (Type SoftwarePrimType SoftwarePrimTypeRep a, SoftwarePrimType a) => SoftwareType a

--------------------------------------------------------------------------------

-- short-hand for the type representation used in our software language.
type SoftwareTypeRep = TypeRep SoftwarePrimType SoftwarePrimTypeRep

-- short-hand for the type representation with functions used in our software language.
type SoftwareTypeRepFun = TypeRepFun SoftwarePrimType SoftwarePrimTypeRep

--------------------------------------------------------------------------------

toSoftwareTypeRep :: Struct SoftwarePrimType c a -> SoftwareTypeRep a
toSoftwareTypeRep = mapStruct (const softwareTypeRep)

witSoftwareType :: SoftwareTypeRep a -> Dict (Typeable a)
witSoftwareType (Single t)
  | Dict <- witSoftwarePrimType t
  = Dict
witSoftwareType (Two ta tb)
  | Dict <- witSoftwareType ta
  , Dict <- witSoftwareType tb
  = Dict

--------------------------------------------------------------------------------
-- ** Software expressions.
--------------------------------------------------------------------------------

-- | ...
data ExtraPrimitive sig
  where
    -- Integer division assuming `divBalanced x y * y == x` (i.e. no remainder).
    -- The purpose of the assumption is to use it in simplifications.
    DivBalanced :: (Integral a, SoftwarePrimType a) =>
        ExtraPrimitive (a :-> a :-> Full a)

    -- Guard a value by an assertion
    GuardVal ::
        AssertionLabel -> String -> ExtraPrimitive (Bool :-> a :-> Full a)

-- | For loop.
data ForLoop sig
  where
    ForLoop :: SoftwareType st =>
        ForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

-- | Interaction with the IO layer.
data Unsafe sig
  where
    -- Turn a program into a pure value
    UnsafePerform :: Software (Exp a) -> Unsafe (Full a)

--------------------------------------------------------------------------------

-- | Software symbols.
type SoftwareConstructs
            =   BindingT
  Syntactic.:+: Let
  Syntactic.:+: Tuple
  Syntactic.:+: SoftwarePrimitive
  Syntactic.:+: ExtraPrimitive
  Syntactic.:+: ForLoop
  Syntactic.:+: Unsafe

-- | Software symbols tagged with their type representation.
type SoftwareDomain = SoftwareConstructs :&: SoftwareTypeRepFun

-- | Software expressons.
newtype Exp a = Exp { unExp :: ASTF SoftwareDomain a }

-- | Evaluate a closed expression
eval :: (Syntactic a, Domain a ~ SoftwareDomain) => a -> Internal a
eval = evalClosed . desugar

--------------------------------------------------------------------------------

-- | Declaring 'Exp' as syntactic sugar.
instance Syntactic (Exp a)
  where
    type Domain (Exp a) = SoftwareDomain
    type Internal (Exp a) = a
    desugar = unExp
    sugar = Exp

instance Syntactic (Struct SoftwarePrimType Exp a)
    -- Note that this instance places no constraints on `a`. This is crucial in
    -- the way it is used in the rest of the code. It would be possible to
    -- define `desugar` and `sugar` in terms of the instance for pairs; however,
    -- that would require constraining `a`.
  where
    type Domain   (Struct SoftwarePrimType Exp a) = SoftwareDomain
    type Internal (Struct SoftwarePrimType Exp a) = a

    desugar (Single a) = unExp a
    desugar (Two a b)  = sugarSymDecor (ValT $ Two ta tb) Pair a' b'
      where
        a' = desugar a
        b' = desugar b
        ValT ta = getDecor a'
        ValT tb = getDecor b'

    sugar a = case getDecor a of
        ValT (Single _)  -> Single $ Exp a
        ValT (Two ta tb) ->
            Two (sugarSymDecor (ValT ta) Fst a) (sugarSymDecor (ValT tb) Snd a)

--------------------------------------------------------------------------------

sugarSymSoftware
  :: ( Signature sig
       , fi             ~ SmartFun SoftwareDomain sig
       , sig            ~ SmartSig fi
       , SoftwareDomain ~ SmartSym fi
       , SyntacticN f fi
       , sub :<: SoftwareConstructs
       , Type SoftwarePrimType SoftwarePrimTypeRep (DenResult sig)
       )
    => sub sig -> f
sugarSymSoftware = sugarSymDecor $ ValT $ typeRep

sugarSymPrimSoftware
    :: ( Signature sig
       , fi             ~ SmartFun SoftwareDomain sig
       , sig            ~ SmartSig fi
       , SoftwareDomain ~ SmartSym fi
       , SyntacticN f fi
       , sub :<: SoftwareConstructs
       , SoftwarePrimType (DenResult sig)
       )
    => sub sig -> f
sugarSymPrimSoftware = sugarSymDecor $ ValT $ Single softwareTypeRep

--------------------------------------------------------------------------------
-- imperative-edsl instances.

instance Imp.FreeExp Exp
  where
    type FreePred Exp = SoftwareType
    constExp = sugarSymSoftware . Lit
    varExp   = sugarSymSoftware . FreeVar

instance Imp.EvalExp Exp
  where
    evalExp = eval

--------------------------------------------------------------------------------
-- syntactic instances.

deriveSymbol    ''ExtraPrimitive
deriveRender id ''ExtraPrimitive

instance Eval ExtraPrimitive
  where
    evalSym DivBalanced = \a b -> if a `mod` b /= 0
      then error $ unwords ["divBalanced", show a, show b, "is not balanced"]
      else div a b
    evalSym (GuardVal _ msg) = \cond a ->
        if cond then a else error $ "Feldspar assertion failure: " ++ msg

instance EvalEnv ExtraPrimitive env

instance Equality ExtraPrimitive
  where
    equal DivBalanced    DivBalanced    = True
    equal (GuardVal _ _) (GuardVal _ _) = True
    equal _ _ = False

    hash DivBalanced    = hashInt 1
    hash (GuardVal _ _) = hashInt 2

instance StringTree ExtraPrimitive

deriveSymbol    ''ForLoop
deriveRender id ''ForLoop
deriveEquality  ''ForLoop

instance Eval ForLoop
  where
    evalSym ForLoop = \len init body ->
        foldl (flip body) init $ genericTake len [0..]

instance EvalEnv ForLoop env

instance StringTree ForLoop

deriveSymbol ''Unsafe

instance Render Unsafe
  where
    renderSym (UnsafePerform _) = "UnsafePerform ..."

instance StringTree Unsafe

instance Eval Unsafe
  where
    evalSym s = error $ "eval: cannot evaluate unsafe operation " ++ renderSym s

instance EvalEnv Unsafe env

-- | 'equal' always returns 'False'
instance Equality Unsafe
  where
    equal _ _ = False

--------------------------------------------------------------------------------
