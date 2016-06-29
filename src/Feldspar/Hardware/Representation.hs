-- | Monad for computations in hardware

module Feldspar.Hardware.Representation where



import Control.Monad.Trans

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware as Hard hiding (Comp(..), Hardware(..))
import Language.Embedded.Imperative.CMD (IxRange(..), Border(..))

import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Frontend

--------------------------------------------------------------------------------
-- * Hardware programs.
--------------------------------------------------------------------------------

-- | Commands used in hardware programs.
type HardwareCMD
    =   SignalCMD
    :+: VariableCMD
    :+: ConstantCMD
    :+: ArrayCMD
    :+: VArrayCMD
    :+: LoopCMD
    :+: ConditionalCMD
    :+: ComponentCMD
    :+: StructuralCMD

-- | ...
newtype Hardware a = Hardware
    { unHardware ::
        ProgramT
          HardwareCMD
          (Param2 HData (PrimType' HPrim))
          (Program CompCMD (Param2 HData (PrimType' HPrim)))
          a
    }
  deriving (Functor, Applicative, Monad)

instance MonadComp HData Hardware
  where
    liftComp        = Hardware . lift . unComp
    iff c t f       = Hardware $ Hard.iff c (unHardware t) (unHardware f)
    for n body      = Hardware $ Hard.for n (unHardware . body)
    while cont body = Hardware $ Hard.while (unHardware cont) (unHardware body)

class Monad m => MonadHardware m
  where
    liftHardware :: m a -> Hardware a

instance MonadHardware (Comp HData) where liftHardware = liftComp
instance MonadHardware Hardware     where liftHardware = id

--------------------------------------------------------------------------------
