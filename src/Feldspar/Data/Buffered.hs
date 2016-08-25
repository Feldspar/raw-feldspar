-- | Double-buffered storage
--
-- This module provides a safer alternative to the methods of the classes
-- 'Manifestable' and 'Manifestable2':
--
-- * 'store' instead of 'manifest'
-- * 'store2' instead of 'manifest2'
-- * 'setStore' instead of 'manifestStore'
-- * 'setStore2' instead of 'manifestStore2'
--
-- Consider the following example:
--
-- > bad = do
-- >   arr  <- newArr 20
-- >   vec1 <- manifest arr (1...20)
-- >   vec2 <- manifest arr $ map (*10) $ reverse vec1
-- >   printf "%d\n" $ sum vec2
--
-- First the vector @(1...20)@ is stored into @arr@. Then the result is used to
-- compute a new vector which is also stored into @arr@. So the storage is
-- updated while it is being read from, leading to unexpected results.
--
-- Using this module, we can make a small change to the program:
--
-- > good = do
-- >   st   <- newStore 20
-- >   vec1 <- store st (1...20)
-- >   vec2 <- store st $ map (*10) $ reverse vec1
-- >   printf "%d\n" $ sum vec2
--
-- Now the program works as expected; i.e. gives the same result as the normal
-- Haskell expression
--
-- > sum $ map (*10) $ reverse [1..20]
--
-- The price we have to pay for safe storage is that @`newStore` l@ allocates
-- twice as much memory as @`newArr` l@. However, none of the other functions in
-- this module allocate any memory.
--
-- Note that this module does not protect against improper use of
-- 'unsafeFreezeStore'. A vector from a frozen 'Store' is only valid as long as
-- the 'Store' is not updated.

module Feldspar.Data.Buffered
  ( Store
  , newStore
  , unsafeFreezeStore
  , unsafeFreezeStore2
  , setStore
  , setStore2
  , store
  , store2
  , loopStore
  , loopStore2
  ) where

-- By only allowing `Store` to be created using `newStore`, we ensure that
-- `unsafeSwapArr` is only used in a safe way (on two arrays allocated in the
-- same scope).



import Prelude ()

import Control.Monad.State

import Feldspar.Representation
import Feldspar.Run
import Feldspar.Data.Vector



-- | Double-buffered storage
data Store a = Store
    { activeBuf :: Arr a
    , freeBuf   :: Arr a
    }

-- | Create a new double-buffered 'Store', initialized to a 0x0 matrix
--
-- This operation allocates two arrays of the given length.
newStore :: (Syntax a, MonadComp m) => Data Length -> m (Store a)
newStore l = Store <$> newNamedArr "store" l <*> newNamedArr "store" l

-- | Read the contents of a 'Store' without making a copy. This is generally
-- only safe if the the 'Store' is not updated as long as the resulting vector
-- is alive.
unsafeFreezeStore :: (Syntax a, MonadComp m) =>
    Data Length -> Store a -> m (Manifest a)
unsafeFreezeStore l = unsafeFreezeSlice l . activeBuf

-- | Read the contents of a 'Store' without making a copy (2-dimensional
-- version). This is generally only safe if the the 'Store' is not updated as
-- long as the resulting vector is alive.
unsafeFreezeStore2 :: (Syntax a, MonadComp m)
    => Data Length  -- ^ Number of rows
    -> Data Length  -- ^ Number of columns
    -> Store a
    -> m (Manifest2 a)
unsafeFreezeStore2 r c Store {..} =
    nest r c <$> unsafeFreezeSlice (r*c) activeBuf

-- | Cheap swapping of the two buffers in a 'Store'
swapStore :: Syntax a => Store a -> Run ()
swapStore Store {..} = unsafeSwapArr activeBuf freeBuf

-- | Write a 1-dimensional vector to a 'Store'. The operation may become a no-op
-- if the vector is already in the 'Store'.
setStore :: (Manifestable Run vec a, Finite vec, Syntax a) =>
    Store a -> vec -> Run ()
setStore st@Store {..} vec = case viewManifest vec of
    Just iarr
      | unsafeEqArrIArr activeBuf iarr ->
          iff (iarrOffset iarr == arrOffset activeBuf)
            (return ())
            saveAndSwap
          -- We don't check if `iarr` is equal to the free buffer, because that
          -- would mean that we're trying to overwrite a frozen buffer while
          -- reading it, which should lead to undefined behavior.
    _ -> saveAndSwap
  where
    saveAndSwap = manifestStore freeBuf vec >> swapStore st

-- | Write a 2-dimensional vector to a 'Store'. The operation may become a no-op
-- if the vector is already in the 'Store'.
setStore2 :: (Manifestable2 Run vec a, Finite2 vec, Syntax a) =>
    Store a -> vec -> Run ()
setStore2 st@Store {..} vec = case viewManifest2 vec of
    Just arr
      | let iarr = unnest arr
      , unsafeEqArrIArr activeBuf iarr ->
          iff (iarrOffset iarr == arrOffset activeBuf)
            (return ())
            saveAndSwap
          -- See comment to `setStore`
    _ -> saveAndSwap
  where
    saveAndSwap = manifestStore2 freeBuf vec >> swapStore st

-- | Write the contents of a vector to a 'Store' and get it back as a
-- 'Manifest' vector
store :: (Manifestable Run vec a, Finite vec, Syntax a) =>
    Store a -> vec -> Run (Manifest a)
store st vec = setStore st vec >> unsafeFreezeStore (length vec) st

-- | Write the contents of a vector to a 'Store' and get it back as a
-- 'Manifest2' vector
store2 :: (Manifestable2 Run vec a, Finite2 vec, Syntax a) =>
    Store a -> vec -> Run (Manifest2 a)
store2 st vec = setStore2 st vec >> unsafeFreezeStore2 r c st
  where
    (r,c) = extent2 vec

loopStore
    :: ( Syntax a
       , Manifestable Run vec1 a
       , Finite vec1
       , Manifestable Run vec2 a
       , Finite vec2
       )
    => Store a
    -> IxRange (Data Length)
    -> (Data Index -> Manifest a -> Run vec1)
    -> vec2
    -> Run (Manifest a)
loopStore st rng body init = do
    setStore st init
    lr <- initRef $ length init
    for rng $ \i -> do
      l    <- unsafeFreezeRef lr
      next <- body i =<< unsafeFreezeStore l st
      setStore st next
      setRef lr $ length next
    l <- unsafeFreezeRef lr
    unsafeFreezeStore l st

loopStore2
    :: ( Syntax a
       , Manifestable2 Run vec1 a
       , Finite2 vec1
       , Manifestable2 Run vec2 a
       , Finite2 vec2
       )
    => Store a
    -> IxRange (Data Length)
    -> (Data Index -> Manifest2 a -> Run vec1)
    -> vec2
    -> Run (Manifest2 a)
loopStore2 st rng body init = do
    setStore2 st init
    rr <- initRef $ numRows init
    cr <- initRef $ numCols init
    for rng $ \i -> do
      r    <- unsafeFreezeRef rr
      c    <- unsafeFreezeRef cr
      next <- body i =<< unsafeFreezeStore2 r c st
      setStore2 st next
      setRef rr $ numRows next
      setRef cr $ numCols next
    r <- unsafeFreezeRef rr
    c <- unsafeFreezeRef cr
    unsafeFreezeStore2 r c st

