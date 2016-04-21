-- | Monad for running Feldspar programs

module Feldspar.Run.Representation where



import Control.Monad.Trans

import Language.Embedded.Imperative as Imp
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
          (Param2 Data PrimType')
          (Program CompCMD (Param2 Data PrimType'))
          a
    }
  deriving (Functor, Applicative, Monad)

instance MonadComp Run
  where
    liftComp        = Run . lift . unComp
    iff c t f       = Run $ Imp.iff c (unRun t) (unRun f)
    for  range body = Run $ Imp.for range (unRun . body)
    while cont body = Run $ Imp.while (unRun cont) (unRun body)

class Monad m => MonadRun m
  where
    liftRun :: m a -> Run a

instance MonadRun Comp where liftRun = liftComp
instance MonadRun Run  where liftRun = id

