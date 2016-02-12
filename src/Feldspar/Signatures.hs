{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Feldspar.Signatures where

import Control.Monad.Operational.Higher

import Language.Syntactic hiding (Signature)

import Feldspar.Representation (Data, SmallType)

--------------------------------------------------------------------------------
-- * Language.
--------------------------------------------------------------------------------

class Argument a

--------------------------------------------------------------------------------

data Signature m a
  where
    Unit
      :: m ()
      -> Signature m ()
         
    Ret
      :: SmallType a
      => m (Data a)
      -> Signature m (Data a)
      
    Lam
      :: (SmallType a, Argument (arg a))
      => (arg a -> Signature m b)
      -> Signature m (arg a -> b)

instance HFunctor Signature
  where
    hfmap f (Unit m) = Unit (f m)
    hfmap f (Ret  a) = Ret  (f a)
    hfmap f (Lam  g) = Lam  (hfmap f . g)

unit :: m () -> Signature m ()
unit = Unit

ret :: SmallType a => m (Data a) -> Signature m (Data a)
ret = Ret

lam :: (SmallType a, Argument (arg a)) => (arg a -> Signature m b) -> Signature m (arg a -> b)
lam = Lam

--------------------------------------------------------------------------------

data FName m a where
  FName :: Maybe String -> Signature m a -> FName m a

type family FResult sig where
  FResult ()       = ()
  FResult (Data a) = Data a
  FResult (a -> b) = FResult b

data FArgument a
  where
    FEmpty
      :: (FResult a ~ a) => FArgument a
         
    FCons
      :: (SmallType a, Argument (arg a))
      => arg a
      -> FArgument b
      -> FArgument (arg a -> b)


nil :: (FResult a ~ a) => FArgument a
nil = FEmpty

cons :: (SmallType a, Argument (arg a)) => arg a -> FArgument b -> FArgument (arg a -> b)
cons = FCons

--------------------------------------------------------------------------------
