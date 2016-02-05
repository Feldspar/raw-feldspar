-- | Monad for computations in hardware

module Feldspar.Hardware.Representation where



import Control.Monad.Trans

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware as Hard

import Feldspar.Representation
import Feldspar.Frontend

--------------------------------------------------------------------------------
-- * Hardware programs.
--------------------------------------------------------------------------------

-- | Commands used in hardware programs.
type HardwareCMD =
      ConditionalCMD Data
  :+: LoopCMD        Data
  :+: SignalCMD      Data
  :+: StructuralCMD  Data

-- | Monad for computations in hardware.
newtype Hardware a = Hardware { unHardware :: ProgramT HardwareCMD (Program CompCMD) a }
  deriving (Functor, Applicative, Monad)

instance MonadComp Hardware
  where
    liftComp           = Hardware . lift . unComp
    iff c t f          = Hardware $ Hard.iff c (unHardware t) (unHardware f)
    for (i, _, _) body = Hardware $ Hard.for i (unHardware . body)
    while cont body    = Hardware $ Hard.while (unHardware cont) (unHardware body)

class Monad m => MonadHardware m
  where
    liftHardware :: m a -> Hardware a

instance MonadHardware Comp     where liftHardware = liftComp
instance MonadHardware Hardware where liftHardware = id

--------------------------------------------------------------------------------
