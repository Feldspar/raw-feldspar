{-# LANGUAGE GADTs #-}

module Feldspar.Signature where

import Feldspar.Representation

--------------------------------------------------------------------------------
-- * Language.
--------------------------------------------------------------------------------

-- | Signature annotations.
data Annotation a
  where
    Empty :: Annotation a

-- | Signatures.
data Signature m a
  where
    Unit    :: Signature m (m ())
    Ret     :: SmallType a => Data a -> Signature m (m (Data a))
    LamData :: SmallType a => (Data a -> Signature m b) -> Signature m (Data a -> b)
    LamRef  :: SmallType a => (Ref a  -> Signature m b) -> Signature m (Ref a  -> b)
    LamArr  :: SmallType a => (Arr a  -> Signature m b) -> Signature m (Arr a  -> b)

--------------------------------------------------------------------------------
-- ** Combinators.

unit :: Signature m (m ())
unit = Unit

ret  :: SmallType a => Data a -> Signature m (m (Data a))
ret = Ret

lamd :: SmallType a => (Data a -> Signature m b) -> Signature m (Data a -> b)
lamd = LamData

lamr :: SmallType a => (Ref a -> Signature m b) -> Signature m (Ref a -> b)
lamr = LamRef

lama :: SmallType a => (Arr a -> Signature m b) -> Signature m (Arr a -> b)
lama = LamArr

--------------------------------------------------------------------------------



{-
f :: D a -> R a -> m (M a)
f = ...

g :: D a -> R a -> A a -> Len -> Len -> m ()
g d r a rows cols = (rows, cols, a) `writeStore` (f d r) 

h = inFunction (lamd ... lamr ... lama ... lamd ... lamd ... g)
-}
