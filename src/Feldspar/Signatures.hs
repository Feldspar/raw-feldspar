{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Feldspar.Signature where

import Language.Syntactic hiding (Signature)

import Feldspar.Representation
import Feldspar.Frontend (MonadComp)

--------------------------------------------------------------------------------
-- * Language.
--------------------------------------------------------------------------------

class Argument a
  where
    mkParam :: a -> ()

--------------------------------------------------------------------------------

data Signature m a
  where
    Unit
      :: m ()
      -> Signature m (m ())
         
    Ret
      :: SmallType a
      => m (Data a)
      -> Signature m (m (Data a))
      
    Lam
      :: (SmallType a, Argument (arg a))
      => (arg a -> Signature m b)
      -> Signature m (arg a -> b)

unit :: m () -> Signature m (m ())
unit = Unit

ret :: SmallType a => m (Data a) -> Signature m (m (Data a))
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

-- | Functions.
data Function sig
  where
    InFunction
      :: Maybe String    -- Function name
      -> Signature m sig -- Signature of the function's type
      -> Function (FunName m sig)
      
    CallFunction
      :: FunName m sig   -- Funtion itself
      -> FunArg  m sig   -- Arguments to functions
      -> Function (FunResult m sig)


inFunction :: MonadComp m => Signature m sig -> m (FunName m sig)
inFunction = undefined

callFunction :: MonadComp m => FunName m sig -> FunArg m sig -> m (FunResult m sig)
callFunction = undefined

--------------------------------------------------------------------------------
