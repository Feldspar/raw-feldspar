{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Compile.Hardware where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader

import qualified Control.Monad.Operational.Higher as H

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import Data.TypeRep
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord

import Language.Embedded.Hardware (HType)
import qualified Language.Embedded.Hardware as Hard

import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft

import Data.VirtualContainer

import Feldspar.Representation hiding (Program)
import Feldspar.Optimize
import Feldspar.Compile.Lower
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend       as Feld

--------------------------------------------------------------------------------
-- * Target language.
--------------------------------------------------------------------------------

type VExp = Virtual SmallType Hard.HExp

data VExp_ where
  VExp_ :: Type a => VExp a -> VExp_

type TargetCMD =
        Hard.SignalCMD      Hard.HExp
  H.:+: Hard.VariableCMD    Hard.HExp
  H.:+: Hard.ArrayCMD       Hard.HExp
  H.:+: Hard.LoopCMD        Hard.HExp
  H.:+: Hard.ConditionalCMD Hard.HExp
  H.:+: Hard.StructuralCMD  Hard.HExp

type Env = Map Name VExp_

-- | Target monad for translation
type Target = ReaderT Env (H.Program TargetCMD)

--------------------------------------------------------------------------------

newRefV :: VirtualType SmallType a => Target (Virtual SmallType Hard.Variable a)
newRefV = Reader.lift $ mapVirtualA (const Hard.newVariable_) virtRep

initRefV :: VirtualType SmallType a => VExp a -> Target (Virtual SmallType Hard.Variable a)
initRefV = Reader.lift . mapVirtualA (Hard.newVariable)

getRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
getRefV = Reader.lift . mapVirtualA (Hard.getVariable)

setRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> VExp a -> Target ()
setRefV r = Reader.lift . sequence_ . zipListVirtual (Hard.setVariable) r

unsafeFreezeRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
unsafeFreezeRefV = Reader.lift . mapVirtualA Hard.unsafeFreezeVariable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type family Harden a
type instance Harden (Soft.Ref   a) = Hard.Variable a
type instance Harden (Soft.Arr i a) = Hard.Array i a
type instance Harden (Data a)       = Data a
type instance Harden ()             = ()

class Lower instr
  where
    lowerInstr :: instr Target a -> Target (Harden a)

instance Lower (Soft.RefCMD Data)
  where
    lowerInstr (Soft.NewRef)     = Reader.lift   Hard.newVariable_
    lowerInstr (Soft.InitRef a)  = Reader.lift . Hard.newVariable               =<< translateSmallExp a
    lowerInstr (Soft.SetRef r a) = Reader.lift . Hard.setVariable (hardenRef r) =<< translateSmallExp a
    lowerInstr (Soft.GetRef r)   = fmap liftVar $ Reader.lift $ Hard.getVariable $ hardenRef r

instance Lower (Soft.ArrCMD Data)
  where
    lowerInstr (Soft.NewArr i)         = Reader.lift . Hard.newArray =<< translateSmallExp i
    lowerInstr (Soft.NewArr_)          = error "lower: hardware arrays must have a length."
    lowerInstr (Soft.InitArr xs)       = Reader.lift $ Hard.initArray xs
    lowerInstr (Soft.SetArr i v a)     = do
      i' <- translateSmallExp i
      v' <- translateSmallExp v
      Reader.lift $ Hard.setArray i' v' (hardenArray a)
    lowerInstr (Soft.GetArr i a)       = do
      i' <- translateSmallExp i
      Reader.lift $ Hard.getArray i'    (hardenArray a)
    lowerInstr (Soft.CopyArr a b l)    = undefined
    lowerInstr (Soft.UnsafeGetArr i a) = undefined

instance (Lower i, Lower j) => Lower (i H.:+: j)
  where
    lowerInstr (H.Inl i) = lowerInstr i
    lowerInstr (H.Inr j) = lowerInstr j

liftVar :: SmallType a => Hard.HExp a -> Data a
liftVar = undefined

-- | Transforms a software reference into a hardware variable.
hardenRef :: SmallType a => Soft.Ref a -> Hard.Variable a
hardenRef (Soft.RefComp i) = Hard.VariableC i
hardenRef (Soft.RefEval i) = Hard.VariableE i

-- | Transforms a software array into a hardware one.
hardenArray :: SmallType a => Soft.Arr i a -> Hard.Array i a
hardenArray (Soft.ArrComp ('a':i)) = Hard.ArrayC (read i :: Integer)
hardenArray (Soft.ArrEval i)       = Hard.ArrayE i

--------------------------------------------------------------------------------

lower :: (Lower instr, H.HFunctor instr) => H.Program instr a -> Target a
lower = H.interpretWithMonad undefined

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

translateSmallExp :: SmallType a => Data a -> Target (Hard.HExp a)
translateSmallExp = undefined

--------------------------------------------------------------------------------
-- Stuff
--------------------------------------------------------------------------------

pHType :: Proxy HType
pHType = Proxy

instance ShowClass HType where showClass _ = "HType"

deriveWitness ''HType ''BoolType
deriveWitness ''HType ''FloatType
deriveWitness ''HType ''DoubleType
deriveWitness ''HType ''IntWordType

derivePWitness ''HType ''BoolType
derivePWitness ''HType ''FloatType
derivePWitness ''HType ''DoubleType
derivePWitness ''HType ''IntWordType

instance PWitness HType CharType t
instance PWitness HType ListType t
instance PWitness HType TupleType t
instance PWitness HType FunType t

-- ...

--------------------------------------------------------------------------------
