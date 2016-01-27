-- | Monad for computations in hardware

module Feldspar.Hardware.Representation where



import Control.Monad.Trans

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware

import Feldspar.Representation



type HardwareCMD =
      ConditionalCMD Data
  :+: LoopCMD        Data
  :+: SignalCMD      Data
  :+: StructuralCMD  Data

-- | Monad for computations in hardware
newtype Hardware a = Hardware { unHardware :: ProgramT HardwareCMD (Program CompCMD) a }
  deriving (Functor, Applicative, Monad)

liftH :: Comp a -> Hardware a
liftH = Hardware . lift . unComp

