-- | Monad for computations in software

module Feldspar.Software.Representation where


import Control.Monad.Operational.Higher
import Control.Monad.Trans

import Language.Embedded.Imperative as Imp hiding (FunArg)

import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Signatures

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Functions.

data FunctionCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ ...
    AddFun  :: Signature prog a -> FunctionCMD exp prog (Maybe String)

    -- ^ ...
    CallFun :: FName prog a -> FArgument a -> FunctionCMD exp prog (FResult a)

type instance IExp (FunctionCMD e)       = e
type instance IExp (FunctionCMD e :+: i) = e

instance HFunctor (FunctionCMD exp)
  where
    hfmap f (AddFun s)              = AddFun (hfmap f s)
    hfmap f (CallFun (FName n s) a) = CallFun (FName n (hfmap f s)) a

--------------------------------------------------------------------------------
-- **

type SoftwareCMD
    =   ControlCMD Data
    :+: PtrCMD     Data
    :+: CallCMD    Data
    :+: ObjectCMD  Data
    :+: FileCMD    Data

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

--------------------------------------------------------------------------------
