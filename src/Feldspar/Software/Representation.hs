-- | Monad for computations in software

module Feldspar.Software.Representation where



import Control.Monad.Trans

import Language.Embedded.Imperative

import Feldspar.Representation



type SoftwareCMD
    =   ControlCMD Data
    :+: PtrCMD
    :+: CallCMD    Data
    :+: ObjectCMD  Data
    :+: FileCMD    Data

-- | Monad for computations in software
newtype Software a = Software { unSoftware :: ProgramT SoftwareCMD (Program CompCMD) a }
  deriving (Functor, Applicative, Monad)

liftS :: Comp a -> Software a
liftS = Software . lift . unComp

