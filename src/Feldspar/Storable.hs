-- | Storable types

module Feldspar.Storable where



import Control.Monad
import Data.Proxy

import Feldspar.Representation
import Feldspar.Frontend



--------------------------------------------------------------------------------
-- * 'Forcible' class
--------------------------------------------------------------------------------

-- | Expression types that can be \"forced\" to values
class Forcible a
  where
    -- | Representation of a forced value
    type ValueRep a :: *

    -- | Force an expression to a value. The resulting value can be used
    -- multiple times without risking re-computation.
    --
    -- 'toValue' will allocate memory to hold the value.
    toValue :: (MonadComp exp m, ExprOf a ~ exp) => a -> m (ValueRep a)

    -- | Convert a forced value back to an expression
    fromValue :: ValueRep a -> a

-- To some extent `Forcible` is subsumed by `Storable`. However, `ValueRep` is
-- more convenient to deal with for the user than `StoreRep`, since the latter
-- consists of mutable data structures (e.g. `Ref a` instead of `Data a`).

-- `Forcible` also resembles the `Syntactic` class, with the difference that the
-- former has a monadic interface and a more free internal representation
-- (`Syntactic` only allows `Data (Internal a)` as the internal representation).
--
-- This difference has two main benefits:
--
--   * We can guarantee that `toValue` returns a "cheep" value. There is no such
--     guarantee for `desugar` of the `Syntactic` class.
--   * We can use data structures such as `IArr` as the representation of values

instance Type a => Forcible (Data a)
  where
    type ValueRep (Data a) = Data a
    toValue   = unsafeFreezeRef <=< initRef
    fromValue = sugar

instance HType a => Forcible (HData a)
  where
    type ValueRep (HData a) = HData a
    toValue   = unsafeFreezeRef <=< initRef
    fromValue = sugar

instance ( Forcible a
         , Forcible b
         , ExprOf a ~ ExprOf b)
    => Forcible (a,b)
  where
    type ValueRep (a,b) = (ValueRep a, ValueRep b)
    toValue (a,b)   = (,) <$> toValue a <*> toValue b
    fromValue (a,b) = (fromValue a, fromValue b)

instance ( Forcible a
         , Forcible b
         , Forcible c
         , ExprOf a ~ ExprOf b
         , ExprOf a ~ ExprOf c)
    => Forcible (a,b,c)
  where
    type ValueRep (a,b,c) = (ValueRep a, ValueRep b, ValueRep c)
    toValue (a,b,c)   = (,,) <$> toValue a <*> toValue b <*> toValue c
    fromValue (a,b,c) = (fromValue a, fromValue b, fromValue c)

instance ( Forcible a
         , Forcible b
         , Forcible c
         , Forcible d
         , ExprOf a ~ ExprOf b
         , ExprOf a ~ ExprOf c
         , ExprOf a ~ ExprOf d)
    => Forcible (a,b,c,d)
  where
    type ValueRep (a,b,c,d) = (ValueRep a, ValueRep b, ValueRep c, ValueRep d)
    toValue (a,b,c,d)   = (,,,) <$> toValue a <*> toValue b <*> toValue c <*> toValue d
    fromValue (a,b,c,d) = (fromValue a, fromValue b, fromValue c, fromValue d)

instance Forcible a => Forcible [a]
  where
    type ValueRep [a] = [ValueRep a]
    toValue   = mapM toValue
    fromValue = map fromValue

-- | Cast between 'Forcible' types that have the same value representation
forceCast
  :: ( Forcible a
     , Forcible b
     , ValueRep a ~ ValueRep b
     , ExprOf a ~ ExprOf b
     , MonadComp (ExprOf a) m)
  => a -> m b
forceCast = fmap fromValue . toValue

-- | Force the computation of an expression. The resulting value can be used
-- multiple times without risking re-computation.
force
  :: ( Forcible a
     , MonadComp (ExprOf a) m)
  => a -> m a
force = forceCast

--------------------------------------------------------------------------------
-- * 'Storable' class
--------------------------------------------------------------------------------

-- | Storable types
class Storable a
  where
    -- | Memory representation
    type StoreRep a
    -- | Size of memory representation
    type StoreSize a

    -- | Creat a fresh memory store. It is usually better to use 'newStore'
    -- instead of this function as it improves type inference.
    newStoreRep :: MonadComp (ExprOf a) m => proxy a -> StoreSize a -> m (StoreRep a)

    -- | Store a value to a fresh memory store. It is usually better to use
    -- 'initStore' instead of this function as it improves type inference.
    initStoreRep :: MonadComp (ExprOf a) m => a -> m (StoreRep a)

    -- | Read from a memory store. It is usually better to use 'readStore'
    -- instead of this function as it improves type inference.
    readStoreRep :: MonadComp (ExprOf a) m => StoreRep a -> m a

    -- | Unsafe freezing of a memory store. It is usually better to use
    -- 'unsafeFreezeStore' instead of this function as it improves type
    -- inference.
    unsafeFreezeStoreRep :: MonadComp (ExprOf a) m => StoreRep a -> m a

    -- | Write to a memory store. It is usually better to use 'writeStore'
    -- instead of this function as it improves type inference.
    writeStoreRep :: MonadComp (ExprOf a) m => StoreRep a -> a -> m ()

    -- | Copy the contents of a store to another store. It is usually better to
    -- use 'copyStore' instead of this function as it improves type inference.
    copyStoreRep :: MonadComp (ExprOf a) m
        => proxy a
        -> StoreRep a  -- ^ Destination
        -> StoreRep a  -- ^ Source
        -> m ()

instance Type a => Storable (Data a)
  where
    type StoreRep (Data a)  = Ref Data a
    type StoreSize (Data a) = ()
    newStoreRep _ _      = newRef
    initStoreRep         = initRef
    readStoreRep         = getRef
    unsafeFreezeStoreRep = unsafeFreezeRef
    writeStoreRep        = setRef
    copyStoreRep _ dst src =
        setRef src . (id :: Data a -> Data a) =<< unsafeFreezeRef dst

instance HType a => Storable (HData a)
  where
    type StoreRep (HData a)  = Ref HData a
    type StoreSize (HData a) = ()
    newStoreRep _ _      = newRef
    initStoreRep         = initRef
    readStoreRep         = getRef
    unsafeFreezeStoreRep = unsafeFreezeRef
    writeStoreRep        = setRef
    copyStoreRep _ dst src =
        setRef src . (id :: HData a -> HData a) =<< unsafeFreezeRef dst

instance ( Storable a
         , Storable b
         , ExprOf a ~ ExprOf b)
    => Storable (a,b)
  where
    type StoreRep (a,b)  = (StoreRep a, StoreRep b)
    type StoreSize (a,b) = (StoreSize a, StoreSize b)
    newStoreRep _ (a,b)          = (,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b
    initStoreRep (a,b)           = (,) <$> initStoreRep a <*> initStoreRep b
    readStoreRep (la,lb)         = (,) <$> readStoreRep la <*> readStoreRep lb
    unsafeFreezeStoreRep (la,lb) = (,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb
    writeStoreRep (la,lb) (a,b)  = writeStoreRep la a >> writeStoreRep lb b
    copyStoreRep _ (la1,lb1) (la2,lb2) = do
        copyStoreRep (Proxy :: Proxy a) la1 la2
        copyStoreRep (Proxy :: Proxy b) lb1 lb2

instance ( Storable a
         , Storable b
         , Storable c
         , ExprOf a ~ ExprOf b
         , ExprOf a ~ ExprOf c)
    => Storable (a,b,c)
  where
    type StoreRep (a,b,c)  = (StoreRep a, StoreRep b, StoreRep c)
    type StoreSize (a,b,c) = (StoreSize a, StoreSize b, StoreSize c)
    newStoreRep _ (a,b,c)            = (,,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b <*> newStoreRep (Proxy :: Proxy c) c
    initStoreRep (a,b,c)             = (,,) <$> initStoreRep a <*> initStoreRep b <*> initStoreRep c
    readStoreRep (la,lb,lc)          = (,,) <$> readStoreRep la <*> readStoreRep lb <*> readStoreRep lc
    unsafeFreezeStoreRep (la,lb,lc)  = (,,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb <*> unsafeFreezeStoreRep lc
    writeStoreRep (la,lb,lc) (a,b,c) = writeStoreRep la a >> writeStoreRep lb b >> writeStoreRep lc c
    copyStoreRep _ (la1,lb1,lc1) (la2,lb2,lc2) = do
        copyStoreRep (Proxy :: Proxy a) la1 la2
        copyStoreRep (Proxy :: Proxy b) lb1 lb2
        copyStoreRep (Proxy :: Proxy c) lc1 lc2

instance ( Storable a
         , Storable b
         , Storable c
         , Storable d
         , ExprOf a ~ ExprOf b
         , ExprOf a ~ ExprOf c
         , ExprOf a ~ ExprOf d)
    => Storable (a,b,c,d)
  where
    type StoreRep (a,b,c,d)  = (StoreRep a, StoreRep b, StoreRep c, StoreRep d)
    type StoreSize (a,b,c,d) = (StoreSize a, StoreSize b, StoreSize c, StoreSize d)
    newStoreRep _ (a,b,c,d)               = (,,,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b <*> newStoreRep (Proxy :: Proxy c) c <*> newStoreRep (Proxy :: Proxy d) d
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
-- ** User interface

-- | Memory for storing values
newtype Store a = Store { unStore :: StoreRep a }
  -- The reason for this type and its associated interface is to improve type
  -- inference over the methods in the `Storable` class. The problem with those
  -- methods is that they involve type families.

-- | Create a fresh 'Store'
newStore :: forall a m . (Storable a, MonadComp (ExprOf a) m) => StoreSize a -> m (Store a)
newStore = fmap Store . newStoreRep (Proxy :: Proxy a)

-- | Store a value to a fresh 'Store'
initStore :: (Storable a, MonadComp (ExprOf a) m) => a -> m (Store a)
initStore = fmap Store . initStoreRep

-- | Read from a 'Store'
readStore :: (Storable a, MonadComp (ExprOf a) m) => Store a -> m a
readStore = readStoreRep . unStore

-- | Unsafe freezeing of a 'Store'. This operation is only safe if the 'Store'
-- is not updated as long as the resulting value is alive.
unsafeFreezeStore :: (Storable a, MonadComp (ExprOf a) m) => Store a -> m a
unsafeFreezeStore = unsafeFreezeStoreRep . unStore

-- | Write to a 'Store'
writeStore :: (Storable a, MonadComp (ExprOf a) m) => Store a -> a -> m ()
writeStore = writeStoreRep . unStore

-- | Copy the contents of a 'Store' to another 'Store'. The size of the data in
-- the source must not exceed the allocated size of the destination store.
copyStore :: (Storable a, MonadComp (ExprOf a) m)
    => Store a  -- ^ Destination
    -> Store a  -- ^ Source
    -> m ()
copyStore dst src = copyStoreRep dst (unStore dst) (unStore src)

-- | Update a 'Store' in-place
inplace :: (Storable a, MonadComp (ExprOf a) m) => Store a -> (a -> a) -> m ()
inplace store f = writeStore store . f =<< unsafeFreezeStore store

--------------------------------------------------------------------------------
