{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal representation of Feldspar programs

module Feldspar.Representation where


#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Array ((!))
import Data.List (genericTake)
import Data.Word

import Language.Syntactic            hiding (Lam, Signature)
import Language.Syntactic.Functional hiding (Lam)
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import Data.TypeRep
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Basic.Typeable ()
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.Tuple.Typeable ()
import Data.TypeRep.Types.IntWord
import Data.TypeRep.Types.IntWord.Typeable ()
import Language.Syntactic.TypeRep (sugarSymTR)
import Language.Syntactic.TypeRep.Sugar.BindingTR ()
import Language.Syntactic.TypeRep.Sugar.TupleTR ()

import qualified Control.Monad.Operational.Higher as H

import Language.Embedded.Hardware (HType)
import Language.Embedded.Hardware.Interface (PredicateExp)

import Language.Embedded.CExp (CType)
import Language.Embedded.Expression (VarPred, EvalExp (..))
import qualified Language.Embedded.Imperative.CMD as Imp

import Data.VirtualContainer

--------------------------------------------------------------------------------
-- * Object-language types
--------------------------------------------------------------------------------

type FeldTypes
    =   BoolType
    :+: FloatType
    :+: DoubleType
    :+: IntWordType
    :+: TupleType
    :+: FunType

pFeldTypes :: Proxy FeldTypes
pFeldTypes = Proxy

-- | Feldspar types
class    (Typeable FeldTypes a, VirtualType SmallType a, Show a, Eq a, Ord a) => Type a
instance (Typeable FeldTypes a, VirtualType SmallType a, Show a, Eq a, Ord a) => Type a

-- | Small Feldspar types
class    (Type a, CType a, HType a) => SmallType a
instance (Type a, CType a, HType a) => SmallType a

instance ShowClass Type      where showClass _ = "Type"
instance ShowClass SmallType where showClass _ = "SmallType"

pType :: Proxy Type
pType = Proxy

pSmallType :: Proxy SmallType
pSmallType = Proxy

type Length = Word32
type Index  = Word32

-- | Mutable variable
newtype Ref a = Ref { unRef :: Virtual SmallType Imp.Ref a }

-- | Mutable array
newtype Arr a = Arr { unArr :: Virtual SmallType (Imp.Arr Index) a }

-- | Immutable array
newtype IArr a = IArr { unIArr :: Virtual SmallType (Imp.IArr Index) a }



--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

-- | Primitive operations
data Primitive sig
  where
    Add :: (SmallType a, Num a) => Primitive (a :-> a :-> Full a)
    Sub :: (SmallType a, Num a) => Primitive (a :-> a :-> Full a)
    Mul :: (SmallType a, Num a) => Primitive (a :-> a :-> Full a)
    Neg :: (SmallType a, Num a) => Primitive (a :-> Full a)
    I2N :: (SmallType a, SmallType b,
            Integral a, Num b)  => Primitive (a :-> Full b)
    Not ::                         Primitive (Bool :-> Full Bool)
    Eq  :: SmallType a          => Primitive (a :-> a :-> Full Bool)
    Lt  :: SmallType a          => Primitive (a :-> a :-> Full Bool)
    Gt  :: SmallType a          => Primitive (a :-> a :-> Full Bool)
    Le  :: SmallType a          => Primitive (a :-> a :-> Full Bool)
    Ge  :: SmallType a          => Primitive (a :-> a :-> Full Bool)

instance Render Primitive
  where
    renderSym Add = "(+)"
    renderSym Sub = "(-)"
    renderSym Mul = "(*)"
    renderSym Neg = "Neg"
    renderSym I2N = "I2N"
    renderSym Not = "Not"
    renderSym Eq  = "(==)"
    renderSym Lt  = "(<)"
    renderSym Gt  = "(>)"
    renderSym Le  = "(<=)"
    renderSym Ge  = "(>=)"
    renderArgs = renderArgsSmart

instance Eval Primitive
  where
    evalSym Add = (+)
    evalSym Sub = (-)
    evalSym Mul = (*)
    evalSym Neg = negate
    evalSym I2N = fromInteger . toInteger
    evalSym Not = not
    evalSym Eq  = (==)
    evalSym Lt  = (<)
    evalSym Gt  = (>)
    evalSym Le  = (<=)
    evalSym Ge  = (>=)

--------------------------------------------------------------------------------
-- ** Array indexing.

data Array sig
  where
    ArrIx :: SmallType a => Imp.IArr Index a -> Array (Index :-> Full a)

instance Render Array
  where
    renderSym (ArrIx (Imp.IArrComp arr)) = "ArrIx " ++ arr
    renderArgs = renderArgsSmart

instance Eval Array
  where
    evalSym (ArrIx (Imp.IArrEval arr)) = (arr!)

instance Equality Array
  where
    equal (ArrIx (Imp.IArrComp arr1)) (ArrIx (Imp.IArrComp arr2)) = arr1 == arr2

--------------------------------------------------------------------------------
-- ** Conditionals.

data Condition sig
  where
    Condition :: Type a => Condition (Bool :-> a :-> a :-> Full a)

instance Eval Condition
  where
    evalSym Condition = \c t f -> if c then t else f

--------------------------------------------------------------------------------
-- ** For loop.

data ForLoop sig
  where
    ForLoop :: Type st => ForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

instance Eval ForLoop
  where
    evalSym ForLoop = \len init body -> foldl (flip body) init $ genericTake len [0..]

--------------------------------------------------------------------------------
-- ** Interaction with the IO layer.
    
data IOSym sig
  where
    -- Result of an IO operation
    FreeVar :: SmallType a => String -> IOSym (Full a)
    -- Turn a program into a pure value
    UnsafePerform :: Comp (Data a) -> IOSym (Full a)
    -- Identity function with a side effect
    UnsafePerformWith :: Comp () -> IOSym (a :-> Full a)
  -- The reason for having `UnsafeArrIx` instead of doing the same thing using
  -- `UnsafePerform` is that `UnsafeArrIx` can be compared for equality, which
  -- may help some optimizations.

instance Render IOSym
  where
    renderSym (FreeVar v)           = v
    renderSym (UnsafePerform _)     = "UnsafePerform ..."
    renderSym (UnsafePerformWith _) = "UnsafePerformWith ..."

instance Eval IOSym
  where
    evalSym (FreeVar v) = error $ "eval: cannot evaluate free variable " ++ v
    evalSym s = error $ "eval: cannot evaluate unsafe operation " ++ renderSym s

-- | 'equal' can only return 'True' for 'FreeVar' and 'UnsafeArrIx'. For
-- 'UnsafeArrIx' it only returns 'True' when the arrays have an intensional
-- representation (i.e. were created to code generation).
instance Equality IOSym
  where
    equal (FreeVar v1) (FreeVar v2) = v1 == v2
    equal _ _ = False

--------------------------------------------------------------------------------

type FeldConstructs
    =   Literal
    :+: BindingT
    :+: Let
    :+: Tuple
    :+: Primitive
    :+: Array
    :+: Condition
    :+: ForLoop
    :+: IOSym

type FeldDomain = FeldConstructs :&: TypeRep FeldTypes

newtype Data a = Data { unData :: ASTF FeldDomain a }

-- | Declaring 'Data' as syntactic sugar
instance Syntactic (Data a)
  where
    type Domain (Data a)   = FeldDomain
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

instance Syntactic (Virtual SmallType Data a)
  where
    type Domain   (Virtual SmallType Data a) = FeldDomain
    type Internal (Virtual SmallType Data a) = a
    desugar = desugar . mapVirtual (ASTFull . unData)
    sugar   = mapVirtual (Data . unASTFull) . sugar

-- | Specialization of the 'Syntactic' class for the Feldspar domain
class    (Syntactic a, Domain a ~ FeldDomain, Type (Internal a)) => Syntax a
instance (Syntactic a, Domain a ~ FeldDomain, Type (Internal a)) => Syntax a

type instance VarPred      Data = SmallType
type instance PredicateExp Data = SmallType

-- | Evaluate an expression
eval :: (Syntactic a, Domain a ~ FeldDomain) => a -> Internal a
eval = evalClosed . desugar
  -- Note that a `Syntax` constraint would rule out evaluating functions

instance EvalExp Data
  where
    litExp = sugarSymTR . Literal
    evalExp = eval



--------------------------------------------------------------------------------
-- * Monadic computations
--------------------------------------------------------------------------------

type CompCMD
  =     Imp.RefCMD     Data
  H.:+: Imp.ArrCMD     Data
  H.:+: Imp.ControlCMD Data

-- | Monad for computational effects: mutable data structures and control flow
newtype Comp a = Comp { unComp :: H.Program CompCMD a }
  deriving (Functor, Applicative, Monad)


--------------------------------------------------------------------------------
-- Uninteresting instances
--------------------------------------------------------------------------------

derivePWitness ''Type ''BoolType
derivePWitness ''Type ''FloatType
derivePWitness ''Type ''DoubleType
derivePWitness ''Type ''IntWordType
derivePWitness ''Type ''TupleType

instance PWitness Type FunType t

derivePWitness ''SmallType ''BoolType
derivePWitness ''SmallType ''FloatType
derivePWitness ''SmallType ''DoubleType
derivePWitness ''SmallType ''IntWordType

instance PWitness SmallType TupleType t
instance PWitness SmallType FunType t

deriveSymbol   ''Primitive
deriveEquality ''Primitive

instance StringTree Primitive

deriveSymbol ''Array

instance StringTree Array

deriveSymbol    ''Condition
deriveRender id ''Condition
deriveEquality  ''Condition

instance StringTree Condition

deriveSymbol    ''ForLoop
deriveRender id ''ForLoop
deriveEquality  ''ForLoop

instance StringTree ForLoop

deriveSymbol ''IOSym

instance StringTree IOSym

instance EvalEnv Primitive env
instance EvalEnv Array env
instance EvalEnv Condition env
instance EvalEnv ForLoop env
instance EvalEnv IOSym env

