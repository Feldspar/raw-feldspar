-- | Monad for computations in software

module Feldspar.Software.Representation where



import Control.Monad.Trans

import Language.Embedded.Imperative as Imp

import Feldspar.Representation
import Feldspar.Frontend



type SoftwareCMD
    =   ControlCMD Data
    :+: PtrCMD
    :+: FileCMD    Data
    :+: C_CMD      Data

-- | Monad for computations in software
newtype Software a = Software { unSoftware :: ProgramT SoftwareCMD (Program CompCMD) a }
  deriving (Functor, Applicative, Monad)

instance MonadComp Software
  where
    liftComp        = Software . lift . unComp
    iff c t f       = Software $ Imp.iff c (unSoftware t) (unSoftware f)
    for  range body = Software $ Imp.for range (unSoftware . body)
    while cont body = Software $ Imp.while (unSoftware cont) (unSoftware body)

class Monad m => MonadSoftware m
  where
    liftSoftware :: m a -> Software a

instance MonadSoftware Comp     where liftSoftware = liftComp
instance MonadSoftware Software where liftSoftware = id

