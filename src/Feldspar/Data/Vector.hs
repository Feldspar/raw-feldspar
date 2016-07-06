{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Feldspar.Data.Vector
  ( module Feldspar.Data.Array
  , module Feldspar.Data.Vector
  ) where



import qualified Prelude

import Control.Monad.Trans
import Data.Proxy

import Feldspar
import Feldspar.Data.Array
import Feldspar.Run
import Feldspar.Run.Concurrent
import qualified Language.Embedded.Concurrent as Imp



-- Motivation for design
-- =============================================================================
--
-- This library is inspired by the different types of vectors in
-- <http://hackage.haskell.org/package/feldspar-language-0.7/docs/Feldspar-Vector-Internal.html>
--
-- However, rather than using Repa-style multi-dimensional vectors, this library
-- only provides one-dimensional structures. Higher dimensions are instead
-- obtained by nesting them. The main reason for this design is to allow
-- interleaving of effects in vector computations. Imagine that we want to apply
-- an operation
--
--     op :: MonadComp m => DPull Double -> m (DPull Double)
--
-- on each row of a matrix `mat :: Pull (DPull Double)`. That gives us:
--
--     fmap op mat :: MonadComp m => Pull (m (DPull Double))
--
-- Notice the interleaved effect `m` between the vector layers. Repa-style
-- vectors cannot have effects interleaved between dimensions.
--
-- Another potential advantage of using nesting is that it's quite possible to
-- have different vector types at different levels. For example
--
--     replicate 5 $ listPush [1,2,3,4 :: Data Double] :: Pull (Push (Data Double))
--
-- is a `Pull` matrix whose rows are `Push`.
--
-- There are also some disadvantages to nesting:
--
--   * There is no guarantee that the inner vectors have the same length, so
--     functions operating on matrices have to assume this property.
--   * Similarly, it is generally not possible to get the length of the inner
--     vectors, so it sometimes has to be provided from the outside (see e.g.
--     `concat`).
--
-- The `PushSeq` type and the `Linearizable` class attempt to overcome some of
-- the limitations by allowing arbitrary nested structures to be flattened
-- without knowing the exact shape of the structure. For example, concatenation
-- of `PushSeq` is done using `join`, which doesn't have the length argument
-- that `concat` has.



--------------------------------------------------------------------------------
-- * Generic operations
--------------------------------------------------------------------------------

-- | Foldable vectors
class Folding vec
  where
    -- | Left fold of a vector
    fold :: Syntax a => (a -> b -> a) -> a -> vec b -> a

    -- | Monadic left fold of a vector
    foldM :: (Syntax a, MonadComp m) => (a -> b -> m a) -> a -> vec b -> m a

-- | Sum the elements of a vector
sum :: (Num a, Syntax a, Folding vec) => vec a -> a
sum = fold (+) 0

-- | Compute the length of a vector using 'fold'. Note that this operation
-- requires traversing the vector.
lengthByFold :: (Functor vec, Folding vec) => vec a -> Data Length
lengthByFold = fold (+) 0 . fmap (const 1)

-- | Perform all actions in a vector in sequence
sequenceVec :: (Folding vec, MonadComp m) => vec (m ()) -> m ()
sequenceVec = foldM (const id) ()



--------------------------------------------------------------------------------
-- * Manifest vectors
--------------------------------------------------------------------------------

-- | A non-nestable vector with a concrete representation in memory
--
-- A multi-dimensional manifest vector can be obtained using 'multiNest'; e.g:
--
-- @
-- -- Vector of Double
-- vec :: `Manifest` (`Data` `Double`)
--
-- -- 10*20 matrix of Double
-- mat :: `Nest` (`Manifest` (`Data` `Double`))
-- mat = `multiNest` (10 `:>` 20 `:>` `ZE`) vec
-- @
--
-- In general, a vector of type @`Nest` ... (`Nest` (`Manifest` a))@ is
-- preferred over @`Pull` ... (`Pull` (`Pull` a))@, because:
--
-- * The former can be converted to the latter (using combinations of `toPull`
--   and `fmap`)
--
-- * The former can be flattened cheaply without using division and modulus (using 'materialize')
--
-- * The former can be used directly (after flattening) in cases where a memory
--   array is needed (e.g. when calling an external procedure). The latter first
--   needs to be copied into a memory array.
newtype Manifest a = Manifest (IArr (Internal a))

type DManifest a = Manifest (Data a)

instance Syntax a => Indexed (Manifest a)
  where
    type IndexedElem (Manifest a) = a
    Manifest arr ! i = sugar (arr ! i)

instance Finite (Manifest a)
  where
    length (Manifest arr) = length arr

instance Slicable (Manifest a)
  where
    slice from n (Manifest arr) = Manifest $ slice from n arr

instance Syntax a => Forcible (Manifest a)
  where
    type ValueRep (Manifest a) = Manifest a
    toValue   = return
    fromValue = id

instance Syntax a => Storable (Manifest a)
  where
    type StoreRep (Manifest a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Manifest a) = Data Length
    newStoreRep _ len = do
        lenRef <- initRef len
        arr    <- newArr len
        return (lenRef,arr)
    initStoreRep vec = do
        rep <- newStoreRep (Nothing :: Maybe (Pull a)) (length vec)
        writeStoreRep rep vec
        return rep
    readStoreRep (lenRef,arr) = do
        len  <- getRef lenRef
        iarr <- slice 0 len <$> freezeArr arr
        return $ Manifest iarr
    unsafeFreezeStoreRep (lenRef,arr) = do
        len  <- unsafeFreezeRef lenRef
        iarr <- slice 0 len <$> unsafeFreezeArr arr
        return $ Manifest iarr
    writeStoreRep (lenRef,dst) (Manifest arr) = do
        setRef lenRef (length arr)
        src <- unsafeThawArr arr
        copyArr dst src
    copyStoreRep _ (dLenRef,dst) (sLenRef,src) = do
        sLen <- unsafeFreezeRef sLenRef
        setRef dLenRef sLen
        copyArr dst (slice 0 sLen src)

instance
    ( MarshalHaskell (Internal a)
    , MarshalFeld (Data (Internal a))
    , Syntax a
    ) =>
      MarshalFeld (Manifest a)
  where
    type HaskellRep (Manifest a) = [Internal a]
    fromFeld (Manifest arr) = fromFeld arr
    toFeld = Manifest <$> toFeld

-- No instance `PushySeq Manifest` because indexing in `Manifest` requires a
-- `Syntax` constraint.



--------------------------------------------------------------------------------
-- * Pull vectors
--------------------------------------------------------------------------------

-- | Pull vector: a vector representation that supports random access and fusion
-- of operations
data Pull a where
    Pull
        :: Data Length        -- Length of vector
        -> (Data Index -> a)  -- Index function
        -> Pull a

-- | 'Pull' vector specialized to 'Data' elements
type DPull a = Pull (Data a)

instance Functor Pull
  where
    fmap f (Pull len ixf) = Pull len (f . ixf)

-- It would be possible to have the instance:
--
--     instance Applicative Pull
--       where
--         pure a  = Pull 1 (const a)
--         Pull len1 ixf1 <*> Pull len2 ixf2 = Pull (len1*len2) $ \i ->
--             let k = i `div` len2
--                 l = i `mod` len2
--             in  ixf1 k $ ixf2 l
--
-- However, it has been omitted due to the use of `div` and `mod`.

instance Indexed (Pull a)
  where
    type IndexedElem (Pull a) = a
    (!) (Pull _ ixf) = ixf

instance Finite (Pull a)
  where
    length (Pull len _) = len

instance Slicable (Pull a)
  where
    slice from n = take n . drop from

instance Syntax a => Forcible (Pull a)
  where
    type ValueRep (Pull a) = Manifest a
    toValue   = toValue . toPush
    fromValue = toPull

instance Syntax a => Storable (Pull a)
  where
    type StoreRep (Pull a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Pull a) = Data Length
    newStoreRep _        = newStoreRep (Proxy :: Proxy (Manifest a))
    initStoreRep         = toValue >=> initStoreRep
    readStoreRep         = fmap fromValue . readStoreRep
    unsafeFreezeStoreRep = fmap fromValue . unsafeFreezeStoreRep
    writeStoreRep s      = toValue >=> writeStoreRep s
    copyStoreRep _       = copyStoreRep (Proxy :: Proxy (Manifest a))

instance
    ( MarshalHaskell (Internal a)
    , MarshalFeld (Data (Internal a))
    , Syntax a
    ) =>
      MarshalFeld (Pull a)
  where
    type HaskellRep (Pull a) = [Internal a]
    fromFeld = fromFeld <=< toValue
    toFeld   = fromValue <$> toFeld
  -- Ideally, we would like the more general instance
  --
  --     instance MarshalFeld a => MarshalFeld (Pull a)
  --       where type HaskellRep (Pull a) = [HaskellRep a]
  --       fromFeld (Pull len ixf) = do
  --           fput stdout "" len " "
  --           for (0,1,Excl len) (fromFeld . ixf)
  --       toFeld = do
  --           len <- fget stdin
  --           return $ Pull len ...
  --
  -- However, `toFeld` needs to return a `Pull` with an index function
  -- `Data Index -> a`. The only way to construct such a function is by storing
  -- the elements in an array and index into that. This poses two problems: (1)
  -- how big an array should `toFeld` allocate, and (2) the type `a` is not
  -- necessarily an expression (e.g. it can be a `Pull`), so there has to be a
  -- way to write `a` to memory, even if it has just been read from memory by
  -- `toFeld`. Problem (1) could be solved if we could have nested mutable
  -- arrays, but problem (2) would remain.

data VecChanSizeSpec lenSpec = VecChanSizeSpec (Data Length) lenSpec

ofLength :: Data Length -> lenSpec -> VecChanSizeSpec lenSpec
ofLength = VecChanSizeSpec

instance ( Syntax a, BulkTransferable a
         , ContainerType a ~ Arr (Internal a)
         ) => Transferable (Pull a)
  where
    type SizeSpec (Pull a) = VecChanSizeSpec (SizeSpec a)
    calcChanSize _ (VecChanSizeSpec n m) =
        let hsz = n `Imp.timesSizeOf` (Proxy :: Proxy Length)
            bsz = calcChanSize (Proxy :: Proxy a) m
        in  hsz `Imp.plusSize` (n `Imp.timesSize` bsz)
    untypedReadChan c = do
        len :: Data Length <- untypedReadChan c
        arr <- newArr len
        untypedReadChanBuf (Proxy :: Proxy a) c 0 len arr
        lenRef <- initRef len
        unsafeFreezeStore $ Store (lenRef, arr)
    untypedWriteChan c v = do
        Store (lenRef, arr) <- initStore v
        len :: Data Length <- getRef lenRef
        untypedWriteChan c len
        untypedWriteChanBuf (Proxy :: Proxy a) c 0 len arr

instance Folding Pull
  where
    fold f x vec = forLoop (length vec) x $ \i s -> f s (vec!i)
      -- It would be possible to also express `fold` by converting to `PushSeq`
      -- (in which case the `Folding` class could probably be replaced by
      -- `PushySeq`), but the current implementation has the advantage of using
      -- a pure `forLoop` which potentially can be better optimized.
    foldM f x vec = foldM f x $ toPushSeq vec

-- | Data structures that are 'Pull'-like (i.e. support '!' and 'length')
class    (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a
instance (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a

-- | Convert a 'Pully' structure (e.g. @`Fin` (`IArr` a)@ or @`Manifest` a@) to
-- a 'Pull' vector
toPull :: Pully vec a => vec -> Pull a
toPull vec = Pull (length vec) (vec!)

instance Pushy Pull
  where
    toPush = pullyToPush

instance PushySeq Pull
  where
    toPushSeq (Pull len ixf) = PushSeq $ \put ->
        for (0,1,Excl len) $ \i -> put (ixf i)



----------------------------------------
-- ** Operations
----------------------------------------

head :: Pully vec a => vec -> a
head = (!0)

tail :: Pully vec a => vec -> Pull a
tail = drop 1

take :: Pully vec a => Data Length -> vec -> Pull a
take l vec = Pull (min (length vec) l) (vec!)

drop :: Pully vec a => Data Length -> vec -> Pull a
drop l vec = Pull (b2i (l<=m) * (m-l)) ((vec!) . (+l))
  where
    m = length vec

splitAt :: Pully vec a => Data Index -> vec -> (Pull a, Pull a)
splitAt l vec = (take l vec, drop l vec)

tails :: Pully vec a => vec -> Pull (Pull a)
tails vec = Pull (length vec + 1) (`drop` vec)

inits :: Pully vec a => vec -> Pull (Pull a)
inits vec = Pull (length vec + 1) (`take` vec)

inits1 :: Pully vec a => vec -> Pull (Pull a)
inits1 = tail . inits

replicate :: Data Length -> a -> Pull a
replicate l = Pull l . const

map :: Pully vec a => (a -> b) -> vec -> Pull b
map f vec = Pull (length vec) (f . (vec!))

zip :: (Pully vec1 a, Pully vec2 b) => vec1 -> vec2 -> Pull (a,b)
zip a b = Pull (length a `min` length b) (\i -> (a!i, b!i))

permute :: Pully vec a =>
    (Data Length -> Data Index -> Data Index) -> (vec -> Pull a)
permute perm vec = Pull len ((vec!) . perm len)
  where
    len = length vec

reverse :: Pully vec a => vec -> Pull a
reverse = permute $ \len i -> len-i-1

(...) :: Data Index -> Data Index -> Pull (Data Index)
l ... h = Pull (b2i (l<h+1) * (h-l+1)) (+l)

infix 3 ...

zipWith :: (Pully vec1 a, Pully vec2 b) =>
    (a -> b -> c) -> vec1 -> vec2 -> Pull c
zipWith f a b = fmap (uncurry f) $ zip a b

-- | Left fold of a non-empty vector
fold1 :: (Syntax a, Pully vec a) => (a -> a -> a) -> vec -> a
fold1 f vec = forLoop (length vec) (vec!0) $ \i st -> f (vec!(i+1)) st

-- | Scalar product
scProd :: (Num a, Syntax a, Pully vec1 a, Pully vec2 a) => vec1 -> vec2 -> a
scProd a b = sum (zipWith (*) a b)



----------------------------------------
-- ** Matrix operations
----------------------------------------

pullMatrix
    :: Data Length  -- ^ Number of rows
    -> Data Length  -- ^ Number of columns
    -> (Data Index -> Data Index -> a)
         -- ^ Index function: @rowIndex -> columnIndex -> element@
    -> Pull (Pull a)
pullMatrix r c ixf = Pull r $ \k -> Pull c $ \l -> ixf k l

-- | Transpose of a matrix. Assumes that the number of rows is > 0 and that all
-- rows have the same length.
transpose :: (Pully mat row, Pully row a) => mat -> Pull (Pull a)
transpose a = Pull (length (a!0)) $ \k -> Pull (length a) $ \l -> a!l!k

-- | Matrix multiplication
matMul
    :: ( Pully mat1 row
       , Pully mat2 row
       , Pully row a
       , Num a
       , Syntax a
       )
    => mat1 -> mat2 -> Pull (Pull a)
matMul a b = forEach a $ \a' ->
               forEach (transpose b) $ \b' ->
                 scProd a' b'
  where
    forEach = flip map



--------------------------------------------------------------------------------
-- * Push vectors
--------------------------------------------------------------------------------

-- | Push vector: a vector representation that supports nested write patterns
-- (e.g. resulting from concatenation) and fusion of operations
--
-- The function that dumps the content of the vector is not allowed to perform
-- any side effects except through the \"write\" method
-- (@`Data` `Index` -> a -> m ()@) that is passed to it. That is, it must hold
-- that @`dumpPush` v (\\_ _ -> `return` ())@ has the same behavior as
-- @`return` ()@.
--
-- This condition ensures that `Push` behaves as pure data with the denotation
-- of a finite list.
data Push a
  where
    Push
        :: Data Length
        -> (forall m . MonadComp m => (Data Index -> a -> m ()) -> m ())
        -> Push a

-- | 'Push' vector specialized to 'Data' elements
type DPush a = Push (Data a)

instance Functor Push
  where
    fmap f (Push len dump) = Push len $ \write ->
        dump $ \i -> write i . f

-- | This instance behaves like the list instance:
--
-- > pure x    = [x]
-- > fs <*> xs = [f x | f <- fs, x <- xs]
instance Applicative Push
  where
    pure a  = Push 1 $ \write -> write 0 a
    Push len1 dump1 <*> Push len2 dump2 = Push (len1*len2) $ \write -> do
        dump2 $ \i2 a ->
          dump1 $ \i1 f ->
            write (i1*len2 + i2) (f a)

-- No instance `Monad Push`, because it's not possible to determine the length
-- of the result of `>>=`.

instance Finite (Push a)
  where
    length (Push len _) = len

instance Syntax a => Forcible (Push a)
  where
    type ValueRep (Push a) = Manifest a
    toValue (Push len dump) = do
        arr <- newArr len
        dump $ \i a -> setArr i a arr
        iarr <- unsafeFreezeArr arr
        return $ Manifest iarr
    fromValue = toPush . (id :: Pull b -> Pull b) . fromValue

instance
    ( MarshalHaskell (Internal a)
    , MarshalFeld (Data (Internal a))
    , Syntax a
    ) =>
      MarshalFeld (Push a)
  where
    type HaskellRep (Push a) = [Internal a]
    fromFeld = fromFeld <=< toValue
    toFeld   = fromValue <$> toFeld

class Pushy vec
  where
    toPush :: vec a -> Push a

instance Pushy Push where toPush = id

-- | Dump the contents of a 'Push' vector
dumpPush :: MonadComp m
    => Push a                     -- ^ Vector to dump
    -> (Data Index -> a -> m ())  -- ^ Function that writes one element
    -> m ()
dumpPush (Push _ dump) = dump

-- | Convert a 'Pully' structure to 'Push'
--
-- This function is useful for vectors that do not have a 'Pushy' instance (e.g.
-- 'Manifest').
pullyToPush :: Pully vec a => vec -> Push a
pullyToPush vec = Push l $ \write -> for (0,1,Excl l) $ \i ->
    write i (vec!i)
  where
    l = length vec



----------------------------------------
-- ** Operations
----------------------------------------

-- | Create a 'Push' vector from a list of elements
listPush :: [a] -> Push a
listPush as = Push 2 $ \write ->
    sequence_ [write (value i) a | (i,a) <- Prelude.zip [0..] as]

-- | Append two vectors to make a 'Push' vector
(++!) :: (Pushy vec1, Pushy vec2) => vec1 a -> vec2 a -> Push a
(++!) v1 v2 =
    let Push len1 dump1 = toPush v1
        Push len2 dump2 = toPush v2
    in  Push (len1+len2) $ \write ->
          dump1 write >> dump2 (write . (+len1))

-- Concatenate nested vectors to a 'Push' vector
concat :: (Pushy vec1, Pushy vec2, Functor vec1)
    => Data Length  -- ^ Length of inner vectors
    -> vec1 (vec2 a)
    -> Push a
concat il vec =
    let Push l dump1 = toPush $ fmap toPush vec
    in  Push (l*il) $ \write ->
          dump1 $ \i (Push l2 dump2) ->
            dump2 $ \j a ->
              write (i*l2+j) a
  -- TODO Assert il==l2

-- | Flatten a vector of elements with a static structure
flattenPush
    :: Pushy vec
    => Data Length  -- ^ Length of the lists returned by the second argument
    -> (a -> [b])   -- ^ Convert source element to a list of destination elements
    -> vec a
    -> Push b
flattenPush n f = concat n . fmap (listPush . f) . toPush



--------------------------------------------------------------------------------
-- * Push vectors with embedded effects
--------------------------------------------------------------------------------

-- | Push vector with embedded effects
data PushE m a
  where
    PushE :: Data Length -> ((Data Index -> a -> m ()) -> m ()) -> PushE m a

instance Functor (PushE m)
  where
    fmap f (PushE len dump) = PushE len $ \write -> dump $ \i -> write i . f

-- | This instance behaves like the list instance:
--
-- > pure x    = [x]
-- > fs <*> xs = [f x | f <- fs, x <- xs]
instance Applicative (PushE m)
  where
    pure a  = PushE 1 $ \write -> write 0 a
    PushE len1 dump1 <*> PushE len2 dump2 = PushE (len1*len2) $ \write -> do
        dump2 $ \i2 a ->
          dump1 $ \i1 f ->
            write (i1*len2 + i2) (f a)

-- No instance `Monad Push`, because it's not possible to determine the length
-- of the result of `>>=`.

-- | Dump the contents of a 'PushE' vector
dumpPushE
    :: PushE m a                  -- ^ Vector to dump
    -> (Data Index -> a -> m ())  -- ^ Function that writes one element
    -> m ()
dumpPushE (PushE _ dump) = dump

-- | Convert a 'PushySeq' vector to 'PushSeqE'
--
-- See 'toFlatPush' for converting all levels of a nested structure to 'PushE'.
toPushE :: MonadComp m => Pushy vec => vec a -> PushE m a
toPushE vec =
    let Push len dump = toPush vec
    in  PushE len dump

-- Concatenate nested 'PushE' vectors
concatPushE
    :: Data Length  -- ^ Length of inner vectors
    -> PushE m (PushE m a)
    -> PushE m a
concatPushE il (PushE l dump1) = PushE (l*il) $ \write ->
      dump1 $ \i (PushE l2 dump2) ->
        dump2 $ \j a ->
          write (i*l2+j) a
  -- TODO Assert il==l2

-- | Join the effect in an element with the internal effect in a 'PushE'
joinElemPush :: Monad m => PushE m (m a) -> PushE m a
joinElemPush (PushE len dump) = PushE len $ \write ->
    dump $ \i m ->
      m >>= write i



--------------------------------------------------------------------------------
-- * Sequential push vectors
--------------------------------------------------------------------------------

-- | Sequential push vector
--
-- Note that 'PushSeq' is a 'Monad', so for example concatenation is done using
-- 'join'.
--
-- The function that dumps the content of the vector is not allowed to perform
-- any side effects except through the \"put\" method (@a -> m ()@) that is
-- passed to it. That is, it must hold that
-- @`dumpPushSeq` v (\\_ -> `return` ())@ has the same behavior as
-- @`return` ()@.
--
-- This condition ensures that `PushSeq` behaves as pure data with the
-- denotation of a (possibly infinite) list.
data PushSeq a
  where
    PushSeq
        :: { dumpPushSeq :: forall m . MonadComp m => (a -> m ()) -> m () }
        -> PushSeq a

-- | 'PushSeq' vector specialized to 'Data' elements
type DPushSeq a = PushSeq (Data a)

instance Functor PushSeq
  where
    fmap f (PushSeq dump) = PushSeq $ \put -> dump (put . f)

instance Applicative PushSeq
  where
    pure  = return
    (<*>) = ap

instance Monad PushSeq
  where
    return a = PushSeq $ \put -> put a
    m >>= k  = PushSeq $ \put ->
        dumpPushSeq m $ \a ->
          dumpPushSeq (k a) put

-- No instance `Syntax a => Forcible (PushSeq a)` because `PushSeq` doesn't have
-- known length.

class PushySeq vec
  where
    toPushSeq :: vec a -> PushSeq a

instance PushySeq PushSeq where toPushSeq = id

instance Folding PushSeq
  where
    fold step init = unsafePerform . foldM (\s -> return . step s) init
      -- `unsafePerform` is safe because of the no-side-effects condition on
      -- `PushSeq`.
    foldM step init vec = do
        r <- initRef init
        let put a = do
              s <- unsafeFreezeRef r
              setRef r =<< step s a
        dumpPushSeq vec put
        unsafeFreezeRef r



----------------------------------------
-- ** Operations
----------------------------------------

-- | Create a 'Push' vector from a list of elements
listPushSeq :: [a] -> PushSeq a
listPushSeq as = PushSeq $ \put -> mapM_ put as

-- | Append two vectors to a 'PushSeq' vector
(++) :: (PushySeq vec1, PushySeq vec2) => vec1 a -> vec2 a -> PushSeq a
(++) v1 v2 = PushSeq $ \put ->
    dumpPushSeq (toPushSeq v1) put >> dumpPushSeq (toPushSeq v2) put

-- | Flatten a vector of elements with a static structure
flatten
    :: PushySeq vec
    => (a -> [b])  -- ^ Convert source element to a list of destination elements
    -> vec a
    -> PushSeq b
flatten f = join . fmap (listPushSeq . f) . toPushSeq

filter :: PushySeq vec => (a -> Data Bool) -> vec a -> PushSeq a
filter pred v = PushSeq $ \put -> do
    let put' a = iff (pred a) (put a) (return ())
    dumpPushSeq (toPushSeq v) put'

unfoldPushSeq :: Syntax a => (a -> (b,a)) -> a -> PushSeq b
unfoldPushSeq step init = PushSeq $ \put -> do
    r <- initRef init
    while (return true) $ do
      s <- unsafeFreezeRef r
      let (a,s') = step s
      put a
      setRef r s'
  -- TODO Make finite



--------------------------------------------------------------------------------
-- * Sequential push vectors with embedded effects
--------------------------------------------------------------------------------

-- | A version of 'PushSeq' with embedded effects
data PushSeqE m a = PushSeqE
    { dumpPushSeqE :: (a -> m ()) -> m () }

instance Functor (PushSeqE m)
  where
    fmap f (PushSeqE dump) = PushSeqE $ \put -> dump (put . f)

instance Applicative (PushSeqE m)
  where
    pure  = return
    (<*>) = ap

instance Monad (PushSeqE m)
  where
    return  = PushSeqE . flip ($)
    m >>= k = PushSeqE $ \put ->
        dumpPushSeqE m $ \a ->
          dumpPushSeqE (k a) put

instance MonadTrans PushSeqE
  where
    lift m = PushSeqE $ \put -> m >>= put

-- | Convert a 'PushySeq' vector to 'PushSeqE'
--
-- See 'linearPush' for converting all levels of a nested structure to
-- 'PushSeqE'.
toPushSeqE :: MonadComp m => PushySeq vec => vec a -> PushSeqE m a
toPushSeqE = PushSeqE . dumpPushSeq . toPushSeq

-- Concatenate nested 'PushSeqE' vectors
concatPushSeqE :: PushSeqE m (PushSeqE m a) -> PushSeqE m a
concatPushSeqE (PushSeqE dump1) = PushSeqE $ \put ->
      dump1 $ \(PushSeqE dump2) ->
        dump2 put

-- | Join the effect in an element with the internal effect in a 'PushSeqE'
joinElemPushSeq :: Monad m => PushSeqE m (m a) -> PushSeqE m a
joinElemPushSeq (PushSeqE dump) = PushSeqE $ \put ->
    dump $ \ m ->
      m >>= put



--------------------------------------------------------------------------------
-- * Materializing nested structures
--------------------------------------------------------------------------------

-- | Returns the dimensionality of a nested structure
--
-- For example:
--
-- @
-- `Dimensions` (`Pull` (`Manifest` (`Data` `Int32`))) = `Dim` (`Dim` ())
-- @
type family Dimensions a
  where
    Dimensions (Comp a)       = Dimensions a
    Dimensions (Run a)        = Dimensions a
    Dimensions (Nest a)       = Dim (Dimensions a)
    Dimensions (Manifest a)   = Dim (Dimensions a)
    Dimensions (Pull a)       = Dim (Dimensions a)
    Dimensions (Push a)       = Dim (Dimensions a)
    Dimensions (PushE m a)    = Dim (Dimensions a)
    Dimensions (PushSeq a)    = Dim (Dimensions a)
    Dimensions (PushSeqE m a) = Dim (Dimensions a)
    Dimensions a              = ()

class DirectMaterializable a
  where
    -- | Convert a structure directly to a flat 'Manifest' without copying. This
    -- method is only supported for types that simply are views of 'Manifest'
    -- vectors; e.g. @`Nest` (`Manifest` a)@.
    maybeToFlatManifest :: Maybe (a -> Manifest (InnerElem a))
    maybeToFlatManifest = Nothing
  -- This class is kept separate from `Materializable` to avoid resolving the
  -- `m` parameter.

-- | Nested structures that can be written to memory
class
    ( DirectMaterializable a
    , Syntax (InnerElem a)
    , MonadComp m
    ) =>
      Materializable m a
  where
    -- | The inner element type of a nested structure
    type InnerElem a

    -- | Convert a nested structure to a flat 'PushE' vector
    toFlatPush
        :: Extent (Dimensions a)  -- ^ Extent of the structure
        -> a                      -- ^ Structure to convert
        -> PushE m (InnerElem a)
    default toFlatPush :: Extent () -> a -> PushE m a
    toFlatPush ZE = pure


instance DirectMaterializable (Data a)
instance DirectMaterializable ()
instance DirectMaterializable (a,b)
instance DirectMaterializable (a,b,c)
instance DirectMaterializable (a,b,c,d)

instance (Type a, MonadComp m)           => Materializable m (Data a)  where type InnerElem (Data a)  = Data a
instance MonadComp m                     => Materializable m ()        where type InnerElem ()        = ()
instance (Syntax (a,b), MonadComp m)     => Materializable m (a,b)     where type InnerElem (a,b)     = (a,b)
instance (Syntax (a,b,c), MonadComp m)   => Materializable m (a,b,c)   where type InnerElem (a,b,c)   = (a,b,c)
instance (Syntax (a,b,c,d), MonadComp m) => Materializable m (a,b,c,d) where type InnerElem (a,b,c,d) = (a,b,c,d)

instance DirectMaterializable (Comp a)

instance Materializable m a => Materializable m (Comp a)
  where
    type InnerElem (Comp a) = InnerElem a
    toFlatPush e m = PushE len $ \write -> do
        a <- liftComp m
        dumpPushE (toFlatPush e a) write
      where
        len = Prelude.product $ listExtent e

instance DirectMaterializable (Run a)

instance Materializable Run a => Materializable Run (Run a)
  where
    type InnerElem (Run a) = InnerElem a
    toFlatPush e m = PushE len $ \write -> do
        a <- m
        dumpPushE (toFlatPush e a) write
      where
        len = Prelude.product $ listExtent e

instance DirectMaterializable (IArr a)
  where
    maybeToFlatManifest = Just Manifest

instance (Type a, MonadComp m) => Materializable m (IArr a)
  where
    type InnerElem (IArr a) = Data a
    toFlatPush e = toPushE . toPull

instance DirectMaterializable a => DirectMaterializable (Nest a)
  where
    maybeToFlatManifest = do
        f <- maybeToFlatManifest
        return (f . unnest)

instance (Materializable m a, Slicable a) => Materializable m (Nest a)
  where
    type InnerElem (Nest a) = InnerElem a
    toFlatPush e = toFlatPush e . pullyToPush

instance DirectMaterializable (Manifest a)
  where
    maybeToFlatManifest = Just id

instance (Syntax a, MonadComp m) => Materializable m (Manifest a)
  where
    type InnerElem (Manifest a) = a
    toFlatPush e = toPushE . toPull

instance DirectMaterializable (Pull a)

instance Materializable m a => Materializable m (Pull a)
  where
    type InnerElem (Pull a) = InnerElem a
    toFlatPush e = toFlatPush e . toPush

instance DirectMaterializable (Push a)

instance Materializable m a => Materializable m (Push a)
  where
    type InnerElem (Push a) = InnerElem a
    toFlatPush e = toFlatPush e . (id :: PushE m a -> PushE m a) . toPushE

instance DirectMaterializable (PushE m a)

instance Materializable m a => Materializable m (PushE m a)
  where
    type InnerElem (PushE m a) = InnerElem a
    toFlatPush (l :> ls) = concatPushE innerLen . fmap (toFlatPush ls)
      where
        innerLen = Prelude.product $ listExtent ls
      -- TODO Assert l == length of argument vector

instance DirectMaterializable (PushSeq a)

instance Materializable m a => Materializable m (PushSeq a)
  where
    type InnerElem (PushSeq a) = InnerElem a
    toFlatPush e
        = toFlatPush e
        . (id :: PushSeqE m a -> PushSeqE m a)
        . toPushSeqE

instance DirectMaterializable (PushSeqE m a)

instance Materializable m a => Materializable m (PushSeqE m a)
  where
    type InnerElem (PushSeqE m a) = InnerElem a
    toFlatPush (l :> ls) (PushSeqE dump) = PushE l $ \write -> do
        r <- initRef (0 :: Data Index)
        dump $ \a -> do
          i <- unsafeFreezeRef r
          dumpPushE (toFlatPush ls a) $ \j b ->
              write (i*innerLen + j) b
          setRef r (i+1)
      where
        innerLen = Prelude.product $ listExtent ls

-- | Convert a nested structure to a flat 'Manifest' vector. The provided
-- storage may or may not be used to hold the result.
--
-- Some example types after supplying the first two arguments (@arr@ is of type
-- @`Arr` `Double`@):
--
-- @
-- `materialize` arr (10 `:>` `ZE`) :: `DPull` `Double` -> `Comp` (`Manifest` (`Data` `Double`))
--   -- 1-dimensional Pull to 1-dimensional Manifest
--
-- `materialize` arr (10 `:>` 20 `:>` 30 `:>` `ZE`) :: `Pull` (`Pull` (`DPush` `Double`)) -> `Comp` (`Manifest` (`Data` `Double`))
--   -- 3-dimensional Pull-Pull-Push to 1-dimensional Manifest
--
-- `materialize` arr (10 `:>` 20 `:>` `ZE`) :: `Pull` (`Comp` (`DPush` `Double`)) -> `Comp` (`Manifest` (`Data` `Double`))
--   -- 2-dimensional Pull-Push /with interleaved effects/ to 1-dimensional Manifest
-- @
--
-- Note the interleaved 'Comp' effect in the last example. In general effects
-- (e.g. 'Comp' or 'Run') are allowed at any level when materializing a nested
-- structure.
materialize :: forall a m . Materializable m a
    => Arr (Internal (InnerElem a))  -- ^ Storage for the resulting vector
    -> Extent (Dimensions a)         -- ^ Extent of the structure
    -> a                             -- ^ Structure to materialize
    -> m (Manifest (InnerElem a))    -- ^ Flat result
materialize _ _ a
    | Just f <- maybeToFlatManifest :: Maybe (a -> Manifest (InnerElem a))
    = return (f a)
materialize arr e a = do
    dump $ \i a -> setArr i (desugar a) arr
    iarr <- unsafeFreezeArr arr
    return $ Manifest iarr
  where
    PushE _len dump = toFlatPush e a
  -- TODO Assert len = ...

-- | Convert a nested structure to a corresponding nested 'Manifest' vector.
-- Memorization can be used to get a structure that supports cheap indexing
-- (i.e. operations overloaded by 'Pully'). The provided storage may or may not
-- be used to hold the result.
--
-- Some example types after supplying the first two arguments (@arr@ is of type
-- @`Arr` `Double`@):
--
-- @
-- `memorize` arr (10 `:>` `ZE`) :: `DPull` `Double` -> `Comp` (`Manifest` (`Data` `Double`))
--   -- 1-dimensional Pull to 1-dimensional Manifest
--
-- `memorize` arr (10 `:>` 20 `:>` 30 `:>` `ZE`) :: `Pull` (`Pull` (`DPush` `Double`)) -> `Comp` (`Nest` (`Nest` (`Manifest` (`Data` `Double`))))
--   -- 3-dimensional Pull-Pull-Push to 3-dimensional Manifest
--
-- `memorize` arr (10 `:>` 20 `:>` `ZE`) :: `Pull` (`Comp` (`DPush` `Double`)) -> `Comp` (`Nest` (`Manifest` (`Data` `Double`)))
--   -- 2-dimensional Pull-Push /with interleaved effects/ to 2-dimensional Manifest
-- @
--
-- Note the interleaved 'Comp' effect in the last example. In general effects
-- (e.g. 'Comp' or 'Run') are allowed at any level when memorizing a nested
-- structure.
memorize :: forall a d m . (Materializable m a, Dimensions a ~ Dim d)
    => Arr (Internal (InnerElem a))
    -> Extent (Dimensions a)
    -> a
    -> m (MultiNest (Dimensions a) (Manifest (InnerElem a)))
memorize arr e = fmap (multiNest e) . materialize arr e

-- | A version of 'memorize' that only stores the structure to the provided
-- array. ('memorize' is not guaranteed to store the structure.)
memorizeStore :: forall a m . Materializable m a
    => Arr (Internal (InnerElem a))  -- ^ Storage for the resulting vector
    -> Extent (Dimensions a)         -- ^ Extent of the structure
    -> a                             -- ^ Structure to materialize
    -> m ()
memorizeStore arr e a = dumpPushE (toFlatPush e a) $ \i a ->
    setArr i (desugar a) arr



--------------------------------------------------------------------------------
-- * Linearizing nested structures
--------------------------------------------------------------------------------

-- Linearizable data structures
class MonadComp m => Linearizable m a
  where
    -- | Convert a structure to a 'PushSeqE' vector
    linearPush :: a -> PushSeqE m (InnerElem a)
    default linearPush :: a -> PushSeqE m a
    linearPush = return

instance MonadComp m => Linearizable m (Data a)
instance MonadComp m => Linearizable m ()
instance MonadComp m => Linearizable m (a,b)
instance MonadComp m => Linearizable m (a,b,c)
instance MonadComp m => Linearizable m (a,b,c,d)
  -- TODO Larger tuples

instance Linearizable m a => Linearizable m (Comp a)
  where
    linearPush = linearPush <=< lift . liftComp

instance Linearizable Run a => Linearizable Run (Run a)
  where
    linearPush = linearPush <=< lift

instance (Type a, MonadComp m) => Linearizable m (IArr a)
  where
    linearPush = linearPush . toPushSeq . toPull

instance (Linearizable m a, InnerElem a ~ a, Syntax a) =>
    Linearizable m (Manifest a)
  where
    linearPush = linearPush . toPushSeq . toPull

instance Linearizable m a => Linearizable m (Pull a)
  where
    linearPush = linearPush . toPushSeq

instance Linearizable m a => Linearizable m (PushSeq a)
  where
    linearPush v = PushSeqE $ \put ->
        dumpPushSeq v $ \a ->
          dumpPushSeqE (linearPush a) put

-- | Fold the content of a 'Linearizable' data structure
linearFold :: (Linearizable m lin, Syntax a, MonadComp m) =>
    (a -> InnerElem lin -> m a) -> a -> lin -> m a
linearFold step init lin = do
    r <- initRef init
    let put a = do
          s <- unsafeFreezeRef r
          setRef r =<< step s a
    dumpPushSeqE (linearPush lin) put
    unsafeFreezeRef r

-- | Map a monadic function over the content of a 'Linearizable' data structure
linearMapM :: (Linearizable m lin, MonadComp m) =>
    (InnerElem lin -> m ()) -> lin -> m ()
linearMapM f = linearFold (\_ -> f) ()

