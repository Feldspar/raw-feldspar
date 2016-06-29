-- | Monad for running Feldspar programs

module Feldspar.Run.Representation where



import Control.Monad.Trans

import Language.Embedded.Imperative as Imp
import Language.Embedded.Expression as Imp
import Language.Embedded.Concurrent

import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Frontend



type RunCMD
    =   ControlCMD
    :+: PtrCMD
    :+: ThreadCMD
    :+: ChanCMD
    :+: FileCMD
    :+: C_CMD

-- | Monad for running Feldspar programs
newtype Run a = Run
    { unRun ::
        ProgramT
          RunCMD
          (Param2 Data (PrimType' Prim))
          (Program CompCMD (Param2 Data (PrimType' Prim)))
          a
    }
  deriving (Functor, Applicative, Monad)

instance MonadComp Data Run
  where
    liftComp        = Run . lift . unComp
    iff c t f       = Run $ Imp.iff c (unRun t) (unRun f)
    for range body  = Run $ Imp.for (Imp.constExp 0, 1, Imp.Incl range) (unRun . body)
    while cont body = Run $ Imp.while (unRun cont) (unRun body)

class Monad m => MonadRun m
  where
    liftRun :: m a -> Run a

instance MonadRun (Comp Data) where liftRun = liftComp
instance MonadRun Run         where liftRun = id

