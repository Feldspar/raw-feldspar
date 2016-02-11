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

type family SignatureM m sig
  where
    SignatureM m ()       = m ()
    SignatureM m (Data a) = m (Data a)
    SignatureM m (a -> b) = a -> SignatureM m b

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

data FunName m a = FunNameC String | FunNameE (Signature m a)

type family FunResult m sig where
  FunResult m (m ())       = m ()
  FunResult m (m (Data a)) = m (Data a)
  FunResult m (a -> b)     = FunResult m b

data FunArg m a
  where
    Empty
      :: (FunResult m a ~ a)
      => FunArg m a
         
    Cons
      :: (SmallType a, Argument (arg a))
      => arg a
      -> FunArg m b
      -> FunArg m (arg a -> b)


nil :: (FunResult m a ~ a) => FunArg m a
nil = Empty

cons :: (SmallType a, Argument (arg a)) => arg a -> FunArg m b -> FunArg m (arg a -> b)
cons = Cons

--------------------------------------------------------------------------------
