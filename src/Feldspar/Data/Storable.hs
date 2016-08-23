-- | Storable types
--
-- Note that the 'Storable' interface is currently not ideal for vectors:
--
-- * The 'Store' representation only allows a vector to be read back as the same
--   type as it was written. But it is usually desired to read a vector as
--   'Manifest' regardless of what type it was written as.
--
--     - But this is solved by using functions that operated on 'StoreRep'
--       instead (such as 'readStoreRep').
--
-- * There is no support for double-buffered storage, as provided by
--   "Feldspar.Data.Buffered" which means that memory management can become more
--   tedious.

module Feldspar.Data.Storable where



import qualified Prelude

import Control.Monad
import Data.Proxy

import Feldspar
import Feldspar.Data.Vector
import Feldspar.Data.Option
import Feldspar.Data.Validated



--------------------------------------------------------------------------------
-- * 'Forcible' class
--------------------------------------------------------------------------------

-- | Expression types that can be \"forced\" to values
class Forcible a
  where
    -- | Representation of a forced value
    type ValueRep a

    -- | Force an expression to a value. The resulting value can be used
    -- multiple times without risking re-computation.
    --
    -- 'toValue' will allocate memory to hold the value.
    toValue :: MonadComp m => a -> m (ValueRep a)

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

instance (Forcible a, Forcible b) => Forcible (a,b)
  where
    type ValueRep (a,b) = (ValueRep a, ValueRep b)
    toValue (a,b)   = (,) <$> toValue a <*> toValue b
    fromValue (a,b) = (fromValue a, fromValue b)

instance (Forcible a, Forcible b, Forcible c) => Forcible (a,b,c)
  where
    type ValueRep (a,b,c) = (ValueRep a, ValueRep b, ValueRep c)
    toValue (a,b,c)   = (,,) <$> toValue a <*> toValue b <*> toValue c
    fromValue (a,b,c) = (fromValue a, fromValue b, fromValue c)

instance (Forcible a, Forcible b, Forcible c, Forcible d) => Forcible (a,b,c,d)
  where
    type ValueRep (a,b,c,d) = (ValueRep a, ValueRep b, ValueRep c, ValueRep d)
    toValue (a,b,c,d)   = (,,,) <$> toValue a <*> toValue b <*> toValue c <*> toValue d
    fromValue (a,b,c,d) = (fromValue a, fromValue b, fromValue c, fromValue d)

instance Forcible a => Forcible [a]
  where
    type ValueRep [a] = [ValueRep a]
    toValue   = Prelude.mapM toValue
    fromValue = Prelude.map fromValue

-- | 'toValue' will force the value even if it's invalid
instance Forcible a => Forcible (Validated a)
  where
    type ValueRep (Validated a) = (Data Bool, ValueRep a)
    toValue (Validated valid a) = toValue (valid,a)
    fromValue = uncurry Validated . fromValue

instance Syntax a => Forcible (Option a)
  where
    type ValueRep (Option a) = (Data Bool, Data (Internal a))
    toValue o = do
        valid <- initRef false
        r     <- initRef (example :: a)
        caseOptionM o
          (\_ -> return ())
          (\b -> setRef valid true >> setRef r b)
        (,) <$> unsafeFreezeRef valid <*> unsafeFreezeRef r
    fromValue (valid,a) = guarded "fromIStore: none" valid (Feldspar.sugar a)
  -- Ideally, one should use `Storable` instead of the `Syntax` constraint, and
  -- make `r` a `Store` instead of a reference. But the problem is that one
  -- would have to make use of `newStore` which needs a size argument. This is
  -- problematic because the size of the value is not known until inside
  -- `caseOptionM`.

-- Note: There are no instances for vector types, because that would require
-- allocating a new array inside `toValue`.

-- | Cast between 'Forcible' types that have the same value representation
forceCast :: (Forcible a, Forcible b, ValueRep a ~ ValueRep b, MonadComp m) =>
    a -> m b
forceCast = fmap fromValue . toValue

-- | Force the computation of an expression. The resulting value can be used
-- multiple times without risking re-computation.
force :: (Forcible a, MonadComp m) => a -> m a
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
    newStoreRep :: MonadComp m => proxy a -> StoreSize a -> m (StoreRep a)

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

instance Storable ()
  where
    type StoreRep ()  = ()
    type StoreSize () = ()
    newStoreRep _ _        = return ()
    initStoreRep _         = return ()
    readStoreRep _         = return ()
    unsafeFreezeStoreRep _ = return ()
    writeStoreRep _ _      = return ()

instance Type a => Storable (Data a)
  where
    type StoreRep (Data a)  = Ref a
    type StoreSize (Data a) = ()
    newStoreRep _ _      = newRef
    initStoreRep         = initRef
    readStoreRep         = getRef
    unsafeFreezeStoreRep = unsafeFreezeRef
    writeStoreRep        = setRef

instance (Storable a, Storable b) => Storable (a,b)
  where
    type StoreRep (a,b)  = (StoreRep a, StoreRep b)
    type StoreSize (a,b) = (StoreSize a, StoreSize b)
    newStoreRep _ (a,b)          = (,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b
    initStoreRep (a,b)           = (,) <$> initStoreRep a <*> initStoreRep b
    readStoreRep (la,lb)         = (,) <$> readStoreRep la <*> readStoreRep lb
    unsafeFreezeStoreRep (la,lb) = (,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb
    writeStoreRep (la,lb) (a,b)  = writeStoreRep la a >> writeStoreRep lb b

instance (Storable a, Storable b, Storable c) => Storable (a,b,c)
  where
    type StoreRep (a,b,c)  = (StoreRep a, StoreRep b, StoreRep c)
    type StoreSize (a,b,c) = (StoreSize a, StoreSize b, StoreSize c)
    newStoreRep _ (a,b,c)            = (,,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b <*> newStoreRep (Proxy :: Proxy c) c
    initStoreRep (a,b,c)             = (,,) <$> initStoreRep a <*> initStoreRep b <*> initStoreRep c
    readStoreRep (la,lb,lc)          = (,,) <$> readStoreRep la <*> readStoreRep lb <*> readStoreRep lc
    unsafeFreezeStoreRep (la,lb,lc)  = (,,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb <*> unsafeFreezeStoreRep lc
    writeStoreRep (la,lb,lc) (a,b,c) = writeStoreRep la a >> writeStoreRep lb b >> writeStoreRep lc c

instance (Storable a, Storable b, Storable c, Storable d) => Storable (a,b,c,d)
  where
    type StoreRep (a,b,c,d)  = (StoreRep a, StoreRep b, StoreRep c, StoreRep d)
    type StoreSize (a,b,c,d) = (StoreSize a, StoreSize b, StoreSize c, StoreSize d)
    newStoreRep _ (a,b,c,d)               = (,,,) <$> newStoreRep (Proxy :: Proxy a) a <*> newStoreRep (Proxy :: Proxy b) b <*> newStoreRep (Proxy :: Proxy c) c <*> newStoreRep (Proxy :: Proxy d) d
    initStoreRep (a,b,c,d)                = (,,,) <$> initStoreRep a <*> initStoreRep b <*> initStoreRep c <*> initStoreRep d
    readStoreRep (la,lb,lc,ld)            = (,,,) <$> readStoreRep la <*> readStoreRep lb <*> readStoreRep lc <*> readStoreRep ld
    unsafeFreezeStoreRep (la,lb,lc,ld)    = (,,,) <$> unsafeFreezeStoreRep la <*> unsafeFreezeStoreRep lb <*> unsafeFreezeStoreRep lc <*> unsafeFreezeStoreRep ld
    writeStoreRep (la,lb,lc,ld) (a,b,c,d) = writeStoreRep la a >> writeStoreRep lb b >> writeStoreRep lc c >> writeStoreRep ld d

initStoreRepVec :: forall m vec
    .  ( Storable vec
       , StoreSize vec ~ Data Length
       , Finite vec
       , MonadComp m
       )
    => vec -> m (StoreRep vec)
initStoreRepVec vec = do
    st <- newStoreRep (Proxy :: Proxy vec) $ length vec
    writeStoreRep st vec
    return st

initStoreRepVec2 :: forall m vec
    .  ( Storable vec
       , StoreSize vec ~ (Data Length, Data Length)
       , Finite2 vec
       , MonadComp m
       )
    => vec -> m (StoreRep vec)
initStoreRepVec2 vec = do
    st <- newStoreRep (Proxy :: Proxy vec) $ extent2 vec
    writeStoreRep st vec
    return st

writeStoreRepVec
    :: ( Manifestable m vec
       , StoreRep (vec a) ~ (Ref Length, Arr (Internal a))
       , Finite (vec a)
       , Syntax a
       , MonadComp m
       )
    => StoreRep (vec a) -> vec a -> m ()
writeStoreRepVec (lr,arr) vec = do
    setRef lr $ length vec
    manifestStore arr vec

writeStoreRepVec2
    :: ( Manifestable2 m vec
       , StoreRep (vec a) ~ (Ref Length, Ref Length, Arr (Internal a))
       , Finite2 (vec a)
       , Syntax a
       , MonadComp m
       )
    => StoreRep (vec a) -> vec a -> m ()
writeStoreRepVec2 (rr,cr,arr) vec = do
    setRef rr $ numRows vec
    setRef cr $ numCols vec
    manifestStore2 arr vec

instance Syntax a => Storable (Manifest a)
  where
    type StoreRep (Manifest a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Manifest a) = Data Length

    newStoreRep _ l = (,) <$> initRef l <*> newArr l

    initStoreRep = initStoreRepVec

    readStoreRep (lr,arr) = do
        l <- getRef lr
        Manifest . slice 0 l <$> freezeArr arr

    unsafeFreezeStoreRep (lr,arr) = do
        l <- unsafeFreezeRef lr
        unsafeFreezeToManifest l arr

    writeStoreRep = writeStoreRepVec

instance Syntax a => Storable (Manifest2 a)
  where
    type StoreRep (Manifest2 a)  = (Ref Length, Ref Length, Arr (Internal a))
    type StoreSize (Manifest2 a) = (Data Length, Data Length)

    newStoreRep _ (r,c) = (,,) <$> initRef r <*> initRef c <*> newArr (r*c)

    initStoreRep = initStoreRepVec2

    readStoreRep (rr,cr,arr) = do
        r <- getRef rr
        c <- getRef cr
        Manifest2 . nest r c . Manifest . slice 0 (r*c) <$> freezeArr arr

    unsafeFreezeStoreRep (rr,cr,arr) = do
        r <- unsafeFreezeRef rr
        c <- unsafeFreezeRef cr
        Manifest2 . nest r c <$> unsafeFreezeToManifest (r*c) arr

    writeStoreRep = writeStoreRepVec2

instance Syntax a => Storable (Pull a)
  where
    type StoreRep (Pull a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Pull a) = Data Length

    newStoreRep _ = newStoreRep (Proxy :: Proxy (Manifest a))
    initStoreRep  = initStoreRepVec

    readStoreRep = fmap (toPull . (id :: Manifest a -> _)) . readStoreRep

    unsafeFreezeStoreRep =
        fmap (toPull . (id :: Manifest a -> _)) . unsafeFreezeStoreRep

    writeStoreRep = writeStoreRepVec

instance Syntax a => Storable (Push Comp a)
  -- Generalizing this instance to any monad would require making the monad a
  -- parameter of the class (like for Manifestable)
  where
    type StoreRep (Push Comp a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Push Comp a) = Data Length

    newStoreRep _ = newStoreRep (Proxy :: Proxy (Manifest a))
    initStoreRep  = initStoreRepVec

    readStoreRep = fmap (toPush . (id :: Manifest a -> _)) . readStoreRep

    unsafeFreezeStoreRep =
        fmap (toPush . (id :: Manifest a -> _)) . unsafeFreezeStoreRep

    writeStoreRep (lr,arr) vec = liftComp $ do
        setRef lr $ length vec
        manifestStore arr vec

instance (Storable a, Syntax a) => Storable (Option a)
  where
    type StoreRep (Option a)  = (Ref Bool, StoreRep a)
    type StoreSize (Option a) = StoreSize a
    newStoreRep _ s = do
        valid <- initRef false
        r     <- newStoreRep (Nothing :: Maybe a) s
        return (valid,r)
    initStoreRep o = do
        valid <- initRef false
        r     <- initStoreRep (example :: a)  -- TODO
        caseOptionM o
          (\_ -> return ())
          (\b -> writeStoreRep (valid,r) (true,b))
        return (valid,r)
    readStoreRep oRep = do
        (valid,a) <- readStoreRep oRep
        return $ guarded "readStoreRep: none" valid a
    unsafeFreezeStoreRep oRep = do
        (valid,a) <- unsafeFreezeStoreRep oRep
        return $ guarded "unsafeFreezeStoreRep: none" valid a
    writeStoreRep oRep@(valid,r) o = caseOptionM o
        (\_ -> setRef valid false)
        (\a -> writeStoreRep oRep (true,a))



----------------------------------------
-- ** User interface
----------------------------------------

-- | Memory for storing values
newtype Store a = Store { unStore :: StoreRep a }
  -- The reason for this type and its associated interface is to improve type
  -- inference over the methods in the `Storable` class. The problem with those
  -- methods is that they involve type families.

-- | Create a fresh 'Store'
newStore :: forall a m . (Storable a, MonadComp m) => StoreSize a -> m (Store a)
newStore = fmap Store . newStoreRep (Proxy :: Proxy a)

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

-- | Update a 'Store' in-place
inplace :: (Storable a, MonadComp m) => Store a -> (a -> a) -> m ()
inplace store f = writeStore store . f =<< unsafeFreezeStore store

