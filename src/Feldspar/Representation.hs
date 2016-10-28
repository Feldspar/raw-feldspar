{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language TypeOperators #-}
{-# language Rank2Types #-}
{-# language ConstraintKinds #-}

{-# language ScopedTypeVariables #-}

{-# language TemplateHaskell #-}

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

import Data.Constraint (Constraint, Dict (..))

-- syntactic.
import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

-- operational-higher.
import qualified Control.Monad.Operational.Higher as Operational

-- imperative-edsl.
import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative.CMD as Imp
import qualified Language.Embedded.Backend.C.Expression as Imp

import Data.Inhabited
import Data.Selection
import Data.TypedStruct

--import Feldspar.Primitive.Representation
--import Feldspar.Primitive.Backend.C ()

--------------------------------------------------------------------------------
-- * Object-language.
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

--------------------------------------------------------------------------------

-- | A selection that includes all labels defined as 'UserAssertion'
onlyUserAssertions :: Selection AssertionLabel
onlyUserAssertions = selectBy $ \l -> case l of
    UserAssertion _ -> True
    _ -> False

--------------------------------------------------------------------------------

-- set of purely computational instructions.
type CompCMD =    Imp.RefCMD
  Operational.:+: Imp.ArrCMD
  Operational.:+: Imp.ControlCMD
  Operational.:+: AssertCMD
  
-- short-hand for programs made of computational instructions.
type Prog expr pred = Operational.Program CompCMD (Operational.Param2 expr pred)

-- | Class of monads that support lifting of computational programs.
class Monad m => MonadComp m
  where
    -- | Expressions.
    type Expr m :: * -> *
    -- | Predicate.
    type Pred m :: * -> Constraint
    -- | Representation of types.
    type TRep m :: * -> *

    -- | Lift a computational progam.
    liftComp :: Prog (Expr m) (Pred m) a -> m a

--------------------------------------------------------------------------------
-- ** Object-language types.
--------------------------------------------------------------------------------

data IntTypeRep a
  where
    Int8Type  :: IntTypeRep Int8
    Int16Type :: IntTypeRep Int16
    Int32Type :: IntTypeRep Int32
    Int64Type :: IntTypeRep Int64

data WordTypeRep a
  where
    Word8Type  :: WordTypeRep Word8
    Word16Type :: WordTypeRep Word16
    Word32Type :: WordTypeRep Word32
    Word64Type :: WordTypeRep Word64

data IntWordTypeRep a
  where
    IntType  :: IntTypeRep a -> IntWordTypeRep a
    WordType :: WordTypeRep a -> IntWordTypeRep a

data FloatingTypeRep a
  where
    FloatType  :: FloatingTypeRep Float
    DoubleType :: FloatingTypeRep Double

data ComplexTypeRep a
  where
    ComplexFloatType  :: ComplexTypeRep (Complex Float)
    ComplexDoubleType :: ComplexTypeRep (Complex Double)

--------------------------------------------------------------------------------

-- | A different view of primitive type representations that allows matching on
--   similar types.
data PrimTypeView a
  where
    PrimTypeBool     :: PrimTypeView Bool
    PrimTypeIntWord  :: IntWordTypeRep a -> PrimTypeView a
    PrimTypeFloating :: FloatingTypeRep a -> PrimTypeView a
    PrimTypeComplex  :: ComplexTypeRep a -> PrimTypeView a

deriving instance Show (IntTypeRep a)
deriving instance Show (WordTypeRep a)
deriving instance Show (IntWordTypeRep a)
deriving instance Show (FloatingTypeRep a)
deriving instance Show (ComplexTypeRep a)
deriving instance Show (PrimTypeView a)

--------------------------------------------------------------------------------

-- | Representation of supported feldspar types as typed binary trees over
--   primitive types.
type TypeRep pred rep = Struct pred rep

-- | Representation of supported value types and N-ary functions over such
--   types.
data TypeRepFun pred rep a
  where
    ValT :: TypeRep pred rep a -> TypeRepFun pred rep a
    FunT :: TypeRep pred rep a -> TypeRepFun pred rep b -> TypeRepFun pred rep (a -> b)

--------------------------------------------------------------------------------

-- | Equality comparison for type signatures.
class PrimTypeEq trep
  where
    primTypeEq :: trep a -> trep b -> Maybe (Dict (a ~ b))

-- | Check whether two type representations are equal.
typeEq :: PrimTypeEq trep => TypeRep pred trep a -> TypeRep pred trep b -> Maybe (Dict (a ~ b))
typeEq (Single t)  (Single u)  = primTypeEq t u
typeEq (Two t1 t2) (Two u1 u2) = do
    Dict <- typeEq t1 u1
    Dict <- typeEq t2 u2
    return Dict
typeEq _ _ = Nothing

-- | Check whether two type representations are equal
typeEqFun :: PrimTypeEq trep => TypeRepFun pred trep a -> TypeRepFun pred trep b -> Maybe (Dict (a ~ b))
typeEqFun (ValT t)     (ValT u)     = typeEq t u
typeEqFun (FunT ta tb) (FunT ua ub) = do
    Dict <- typeEq ta ua
    Dict <- typeEqFun tb ub
    return Dict
typeEqFun _ _ = Nothing

--------------------------------------------------------------------------------

-- | Supported types, that is, types which can be represented as nested pairs of
--   simpler values that respect `pred` and are in turn represented using `trep`.
class (Eq a, Show a, Typeable a, Inhabited a) => Type pred trep a
  where
    typeRep :: TypeRep pred trep a

-- pairs of valid types are themselves also valid types.
instance (Type pred trep a, Type pred trep b) => Type pred trep (a, b)
  where
    typeRep = Two typeRep typeRep

--------------------------------------------------------------------------------
-- ** Object-language expressions.
--------------------------------------------------------------------------------

-- | Alternative representation of expressions to expose nesting of values.
class Syntactic a => Structured pred expr a
  where
    construct :: a -> Struct pred expr (Internal a)
    destruct  :: Struct pred expr (Internal a) -> a

-- every syntactical object with a corresponding syntactical instance as a
-- structure can be made an instance of `Structured` by casting with `resugar`.
instance
    ( Syntactic a
    , Syntactic (Struct pred expr (Internal a))
    , Domain    (Struct pred expr (Internal a)) ~ Domain a
    , Internal  (Struct pred expr (Internal a)) ~ Internal a
    )
      => Structured pred expr a
  where
    construct = resugar
    destruct  = resugar

--------------------------------------------------------------------------------

class (Structured (Pred m) (Expr m) a, Type (Pred m) (TRep m) (Internal a)) => Syntax m a

--------------------------------------------------------------------------------
