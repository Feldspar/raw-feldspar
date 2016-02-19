-- | Storable types

module Feldspar.Storable where



import Control.Monad
import Data.Proxy

import Feldspar.Representation
import Feldspar.Frontend



--------------------------------------------------------------------------------
-- * Class
--------------------------------------------------------------------------------

-- | Storable types
class Storable a
  where
    -- | Memory representation
    type StoreRep a

    -- | Store a value to a fresh memory store. It is usually better to use
    -- 'initStore' instead of this function as it improves type inference.
    initStoreRep :: MonadComp m => a -> m (StoreRep a)

    -- | Read from a memory store. It is usually better to use 'readStore'
    -- instead of this function as it improves type inference.
    readStoreRep :: MonadComp m => StoreRep a -> m a

    -- | Unsafe freezing of a memory store. It is usually better to use
    -- 'unsafeFreezeStore' instead of this function as it improves type
    -- inference.
    unsafeFreezeStoreRep :: MonadComp m => StoreRep a -> m a

    -- | Write to a memory store. It is usually better to use 'writeStore'
    -- instead of this function as it improves type inference.
    writeStoreRep :: MonadComp m => StoreRep a -> a -> m ()

    -- | Copy the contents of a store to another store. It is usually better to
    -- use 'copyStore' instead of this function as it improves type inference.
    copyStoreRep :: MonadComp m
        => proxy a
        -> StoreRep a  -- ^ Destination
        -> StoreRep a  -- ^ Source
        -> m ()

instance Type a => Storable (Data a)
  where
    type StoreRep (Data a) = Ref a
    initStoreRep         = initRef
    readStoreRep         = getRef
    unsafeFreezeStoreRep = getRef
    writeStoreRep        = setRef
    copyStoreRep _ dst src =
        setRef src . (id :: Data a -> Data a) =<< unsafeFreezeRef dst

instance (Storable a, Storable b) => Storable (a,b)
  where
    type StoreRep (a,b) = (StoreRep a, StoreRep b)
    initStoreRep (a,b)           = (,) <$> initStoreRep a <*> initStoreRep b
    readStoreRep (la,lb)         = (,) <$> readStoreRep la <*> readStoreRep lb
    unsafeFreezeStoreRep (la,lb) = (,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb
    writeStoreRep (la,lb) (a,b)  = writeStoreRep la a >> writeStoreRep lb b
    copyStoreRep _ (la1,lb1) (la2,lb2) = do
        copyStoreRep (Proxy :: Proxy a) la1 la2
        copyStoreRep (Proxy :: Proxy b) lb1 lb2

instance (Storable a, Storable b, Storable c) => Storable (a,b,c)
  where
    type StoreRep (a,b,c) = (StoreRep a, StoreRep b, StoreRep c)
    initStoreRep (a,b,c)             = (,,) <$> initStoreRep a <*> initStoreRep b <*> initStoreRep c
    readStoreRep (la,lb,lc)          = (,,) <$> readStoreRep la <*> readStoreRep lb <*> readStoreRep lc
    unsafeFreezeStoreRep (la,lb,lc)  = (,,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb <*> unsafeFreezeStoreRep lc
    writeStoreRep (la,lb,lc) (a,b,c) = writeStoreRep la a >> writeStoreRep lb b >> writeStoreRep lc c
    copyStoreRep _ (la1,lb1,lc1) (la2,lb2,lc2) = do
        copyStoreRep (Proxy :: Proxy a) la1 la2
        copyStoreRep (Proxy :: Proxy b) lb1 lb2
        copyStoreRep (Proxy :: Proxy c) lc1 lc2

instance (Storable a, Storable b, Storable c, Storable d) => Storable (a,b,c,d)
  where
    type StoreRep (a,b,c,d) = (StoreRep a, StoreRep b, StoreRep c, StoreRep d)
    initStoreRep (a,b,c,d)                = (,,,) <$> initStoreRep a <*> initStoreRep b <*> initStoreRep c <*> initStoreRep d
    readStoreRep (la,lb,lc,ld)            = (,,,) <$> readStoreRep la <*> readStoreRep lb <*> readStoreRep lc <*> readStoreRep ld
    unsafeFreezeStoreRep (la,lb,lc,ld)    = (,,,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb <*> unsafeFreezeStoreRep lc <*> unsafeFreezeStoreRep ld
    writeStoreRep (la,lb,lc,ld) (a,b,c,d) = writeStoreRep la a >> writeStoreRep lb b >> writeStoreRep lc c >> writeStoreRep ld d
    copyStoreRep _ (la1,lb1,lc1,ld1) (la2,lb2,lc2,ld2) = do
        copyStoreRep (Proxy :: Proxy a) la1 la2
        copyStoreRep (Proxy :: Proxy b) lb1 lb2
        copyStoreRep (Proxy :: Proxy c) lc1 lc2
        copyStoreRep (Proxy :: Proxy d) ld1 ld2



--------------------------------------------------------------------------------
-- * User interface
--------------------------------------------------------------------------------

-- | Cast between 'Storable' types that have the same memory representation
castStore :: (Storable a, Storable b, StoreRep a ~ StoreRep b, MonadComp m) =>
    a -> m b
castStore = initStoreRep >=> unsafeFreezeStoreRep

-- | Store a value to memory and read it back
store :: (Storable a, MonadComp m) => a -> m a
store = castStore

-- | Memory for storing values
newtype Store a = Store { unStore :: StoreRep a }
  -- The reason for this type and its associated interface is to improve type
  -- inference over the methods in the `Storable` class. The problem with those
  -- methods is that they involve type families.

-- | Store a value to a fresh 'Store'
initStore :: (Storable a, MonadComp m) => a -> m (Store a)
initStore = fmap Store . initStoreRep

-- | Read from a 'Store'
readStore :: (Storable a, MonadComp m) => Store a -> m a
readStore = readStoreRep . unStore

-- | Unsafe freezeing of a 'Store'. This operation is only safe if the 'Store'
-- is not updated as long as the resulting value is alive.
unsafeFreezeStore :: (Storable a, MonadComp m) => Store a -> m a
unsafeFreezeStore = unsafeFreezeStoreRep . unStore

-- | Write to a 'Store'
writeStore :: (Storable a, MonadComp m) => Store a -> a -> m ()
writeStore = writeStoreRep . unStore

-- | Copy the contents of a 'Store' to another 'Store'. The size of the data in
-- the source must not exceed the allocated size of the destination store.
copyStore :: (Storable a, MonadComp m)
    => Store a  -- ^ Destination
    -> Store a  -- ^ Source
    -> m ()
copyStore dst src = copyStoreRep dst (unStore dst) (unStore src)

-- | Update a 'Store' in-place
inplace :: (Storable a, MonadComp m) => Store a -> (a -> a) -> m ()
inplace store f = writeStore store . f =<< unsafeFreezeStore store

