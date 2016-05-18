{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Data.Vector where



import qualified Prelude

import Data.Proxy

import Feldspar
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
-- is a matrix whose rows are `Pull` and whose columns are `Push`.
--
-- There are also some disadvantages to nesting:
--
--   * We can't have nested manifest vectors, so conversion to/from memory
--     arrays becomes a bit ad hoc (see the various `toPull*` functions).
--   * There is no guarantee that the inner vectors have the same length, so
--     functions operating on matrices have to assume squareness.
--   * Similarly, it is generally not possible to get the length of the inner
--     vectors (squareness holding or not), so it sometimes has to be provided
--     from the outside (see e.g. `concatPush`).
--
-- The `PushSeq` type and the `Linearizable` class attempt to overcome some of
-- the limitations by allowing arbitrary nested structures to be flattened
-- without knowing the exact shape of the structure. For example, `concat`
-- (which returns `PushSeq`) doesn't have the length argument that `concatPush`
-- has.



--------------------------------------------------------------------------------
-- * General operations
--------------------------------------------------------------------------------

-- | Foldable vectors
class Folding vec
  where
    -- | Left fold of a vector
    fold :: Syntax a => (a -> b -> a) -> a -> vec b -> a

-- | Sum the elements of a vector
sum :: (Num a, Syntax a, Folding vec) => vec a -> a
sum = fold (+) 0

-- | Compute the length of a vector using 'fold'. Note that this operation
-- requires traversing the vector.
lengthByFold :: (Functor vec, Folding vec) => vec a -> Data Length
lengthByFold = fold (+) 0 . fmap (const 1)



--------------------------------------------------------------------------------
-- * Pull vectors
--------------------------------------------------------------------------------

-- | Pull vector
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

instance Indexed (Pull a)
  where
    type IndexedElem (Pull a) = a
    (!) (Pull _ ixf) = ixf

instance Finite (Pull a)
  where
    length (Pull len _) = len

instance Syntax a => Forcible (Pull a)
  where
    type ValueRep (Pull a) = Dim1 (IArr (Internal a))
    toValue   = fromPull
    fromValue = toPullSyn

instance Syntax a => Storable (Pull a)
  where
    type StoreRep (Pull a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Pull a) = Data Length
    newStoreRep _ len = do
        lenRef <- initRef len
        arr    <- newArr len
        return (lenRef,arr)
    initStoreRep vec = do
        rep <- newStoreRep (Nothing :: Maybe (Pull a)) (length vec)
        writeStoreRep rep vec
        return rep
    readStoreRep (lenRef,arr) = do
        len <- getRef lenRef
        freezeVec len arr
    unsafeFreezeStoreRep (lenRef,arr) = do
        len <- unsafeFreezeRef lenRef
        unsafeFreezeVec len arr
    writeStoreRep (lenRef,arr) (Pull l ixf) =
        for (0, 1, Excl l) $ \i -> setArr i (ixf i) arr
    copyStoreRep _ (dLenRef,dst) (sLenRef,src) = do
        sLen <- unsafeFreezeRef sLenRef
        setRef dLenRef sLen
        copyArr dst src sLen

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) =>
    MarshalFeld (Pull (Data a))
  where
    type HaskellRep (Pull (Data a)) = [a]
    fromFeld = fromPull >=> fromFeld
    toFeld   = toPull <$> (toFeld :: Run (Dim1 (IArr a)))

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



----------------------------------------
-- ** Conversion
----------------------------------------

-- Note the absence of this class:
--
--     class Pully vec where toPull :: vec a -> Pull a
--
-- The reason is that the only structures that can be converted to `Pull` are

class    (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a
instance (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a

-- | Convert an indexed structure to a 'Pull' vector
--
-- Example types:
--
-- @
-- `Dim1` (`IArr` `Double`) -> `Pull` (`Data` `Double`)
-- `Dim2` (`IArr` `Double`) -> `Pull` (`Data` `Double`)
-- @
toPull :: Pully vec a => vec -> Pull a
toPull arr = Pull (length arr) (arr!)

-- | A version of 'toPull' for elements in the 'Syntax' class
toPullSyn
    :: (Indexed arr, Finite arr, Syntax a, IndexedElem arr ~ Data (Internal a))
    => arr -> Pull a
toPullSyn = fmap sugar . toPull

-- | Convert a 2-dimensional indexed array to a nested 'Pull' vector
toPull2 :: (Indexed arr, Syntax a, IndexedElem arr ~ Data (Internal a)) =>
    Dim2 arr -> Pull (Pull a)
toPull2 (Dim2 r c arr) =
    Pull r $ \k ->
      Pull c $ \l ->
        sugar (arr ! (k*c+l))

fromPull :: (Syntax a, MonadComp m) => Pull a -> m (Dim1 (IArr (Internal a)))
fromPull (Pull len ixf) = do
    arr <- newArr len
    for (0,1,Excl len) $ \i -> setArr i (ixf i) arr
    iarr <- unsafeFreezeArr arr
    return $ Dim1 len iarr

freezeVec :: (MonadComp m, Syntax a) =>
    Data Length -> Arr (Internal a) -> m (Pull a)
freezeVec len arr = do
    iarr <- freezeArr arr len
    return $ Pull len $ \i -> arrIx iarr i

unsafeFreezeVec :: (MonadComp m, Syntax a) =>
    Data Length -> Arr (Internal a) -> m (Pull a)
unsafeFreezeVec len arr = do
    iarr <- unsafeFreezeArr arr
    return $ Pull len $ \i -> arrIx iarr i



----------------------------------------
-- ** Vector operations
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

zip :: (Pully vec1 a, Pully vec2 b) => vec1 -> vec2 -> Pull (a,b)
zip a b = Pull (length a `min` length b) (\i -> (a!i, b!i))

unzip :: Pully vec (a,b) => vec -> (Pull a, Pull b)
unzip ab = (Pull len (fst . (ab!)), Pull len (snd . (ab!)))
  where
    len = length ab

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
fold1 f vec  = forLoop (length vec) (vec!0) $ \i st -> f (vec!(i+1)) st

-- | Scalar product
scProd :: (Num a, Syntax a, Pully vec a) => vec -> vec -> a
scProd a b = sum (zipWith (*) a b)



----------------------------------------
-- ** Matrix operations
----------------------------------------

pullMatrix
    :: Data Length  -- ^ Number of rows
    -> Data Length  -- ^ Number of columns
    -> (Data Index -> Data Index -> a)
         -- ^ @row index -> column index -> element@
    -> Pull (Pull a)
pullMatrix r c ixf = Pull r $ \k -> Pull c $ \l -> ixf k l

-- These operations are not overloaded using `Pully` since there are no nested
-- structures that can be converted to `Pull (Pull a)` without going through
-- memory.

-- | Transpose of a matrix. Assumes that the number of rows is > 0 and that the
-- matrix is square.
transpose :: Pull (Pull a) -> Pull (Pull a)
transpose a = Pull (length (a!0)) $ \k -> Pull (length a) $ \l -> a!l!k

-- | Matrix multiplication
matMul :: (Num a, Syntax a) => Pull (Pull a) -> Pull (Pull a) -> Pull (Pull a)
matMul a b = forEach a $ \a' ->
               forEach (transpose b) $ \b' ->
                 scProd a' b'
  where
    forEach = flip fmap



--------------------------------------------------------------------------------
-- * Push vectors
--------------------------------------------------------------------------------

-- | Push vector
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
    fmap f (Push len dump) = Push len $ \write -> dump $ \i -> write i . f

instance Finite (Push a)
  where
    length (Push len _) = len

class Pushy vec
  where
    toPush :: vec a -> Push a

instance Pushy Push where toPush = id

instance Pushy Pull
  where
    toPush (Pull len ixf) = Push len $ \write ->
        for (0,1,Excl len) $ \i ->
          write i (ixf i)



----------------------------------------
-- ** Operations
----------------------------------------

-- | Create a 'Push' vector from a list of elements
listPush :: [a] -> Push a
listPush as = Push 2 $ \write ->
    sequence_ [write (value i) a | (i,a) <- Prelude.zip [0..] as]

-- | Append two vectors to a 'Push' vector
(++!) :: (Pushy vec1, Pushy vec2) => vec1 a -> vec2 a -> Push a
(++!) v1 v2 =
    let Push len1 dump1 = toPush v1
        Push len2 dump2 = toPush v2
    in  Push (len1+len2) $ \write ->
          dump1 write >> dump2 write

-- Concatenate nested vectors to a 'Push' vector
concatPush :: (Pushy vec1, Pushy vec2, Functor vec1)
    => Data Length  -- ^ Length of inner vectors
    -> vec1 (vec2 a)
    -> Push a
concatPush il vec =
    let Push l dump1 = toPush $ fmap toPush vec
    in  Push (l*il) $ \write ->
          dump1 $ \i (Push l2 dump2) ->
            dump2 $ \j a ->
              write (i*l2+j) a

-- | Flatten a vector of elements with a static structure
flattenPush
    :: Pushy vec
    => Data Length  -- ^ Length of the lists returned by the second argument
    -> (a -> [b])   -- ^ Convert source element to a list of destination elements
    -> vec a
    -> Push b
flattenPush n f = concatPush n . fmap (listPush . f) . toPush



--------------------------------------------------------------------------------
-- * Sequential push vectors
--------------------------------------------------------------------------------

-- | Sequential push vector
data PushSeq a
  where
    PushSeq
        :: (forall m . MonadComp m => (a -> m ()) -> m ())
        -> PushSeq a

-- | 'PushSeq' vector specialized to 'Data' elements
type DPushSeq a = PushSeq (Data a)

instance Functor PushSeq
  where
    fmap f (PushSeq dump) = PushSeq $ \put -> dump (put . f)

class PushySeq vec
  where
    toPushSeq :: vec a -> PushSeq a

instance PushySeq PushSeq where toPushSeq = id

instance PushySeq Pull
  where
    toPushSeq (Pull len ixf) = PushSeq $ \put ->
        for (0,1,Excl len) $ \i -> put (ixf i)

instance Folding PushSeq
  where
    fold step init (PushSeq dump) = unsafePerform $ do
        r <- initRef init
        let put = modifyRef r . flip step
        dump put
        unsafeFreezeRef r



----------------------------------------
-- ** Operations
----------------------------------------

-- | Create a 'Push' vector from a list of elements
listPushSeq :: [a] -> PushSeq a
listPushSeq as = PushSeq $ \put -> mapM_ put as

-- | Append two vectors to a 'PushSeq' vector
(++) :: (PushySeq vec1, PushySeq vec2) => vec1 a -> vec2 a -> PushSeq a
(++) v1 v2 =
    let PushSeq dump1 = toPushSeq v1
        PushSeq dump2 = toPushSeq v2
    in  PushSeq $ \put ->
          dump1 put >> dump2 put

-- Concatenate nested vectors to a 'PushSeq' vector
concat :: (PushySeq vec1, PushySeq vec2, Functor vec1) =>
    vec1 (vec2 a) -> PushSeq a
concat vec =
    let PushSeq dump1 = toPushSeq $ fmap toPushSeq vec
    in  PushSeq $ \put ->
          dump1 $ \(PushSeq dump2) ->
            dump2 $ \a ->
              put a

-- | Flatten a vector of elements with a static structure
flatten
    :: PushySeq vec
    => (a -> [b])  -- ^ Convert source element to a list of destination elements
    -> vec a
    -> PushSeq b
flatten f = concat . fmap (listPushSeq . f) . toPushSeq

filter :: PushySeq vec => (a -> Data Bool) -> vec a -> PushSeq a
filter pred v =
    let PushSeq dump = toPushSeq v
    in  PushSeq $ \put -> do
          let put' a = iff (pred a) (put a) (return ())
          dump put'

foldM :: (Syntax a, PushySeq vec, MonadComp m) =>
    (a -> b -> m a) -> a -> vec b -> m a
foldM step init v = do
    let PushSeq dump = toPushSeq v
    r <- initRef init
    let put a = do
          s <- unsafeFreezeRef r
          setRef r =<< step s a
    dump put
    unsafeFreezeRef r

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
-- * Linearization
--------------------------------------------------------------------------------

-- Linearizable data structures
class Linearizable m a
  where
    type LinearElem a

    -- | Given a data structure and a method for pushing an element, put all
    -- elements in sequence
    linearPush
        :: a                              -- ^ Structure to linearize
        -> (Data (LinearElem a) -> m ())  -- ^ Method for pushing a single element
        -> m ()
             -- Note: This is `PushSeq` with `m` exposed. It needs to be exposed
             -- for the instances for monads to work.

instance Linearizable m (Data a)
  where
    type LinearElem (Data a) = a
    linearPush = flip ($)

instance (Syntax (a,b), Monad m) => Linearizable m (a,b)
  where
    type LinearElem (a,b) = Internal (a,b)
    linearPush a put = linearPush (desugar a) put

instance (Syntax (a,b,c), Monad m) => Linearizable m (a,b,c)
  where
    type LinearElem (a,b,c) = Internal (a,b,c)
    linearPush a put = linearPush (desugar a) put

instance (Syntax (a,b,c,d), Monad m) => Linearizable m (a,b,c,d)
  where
    type LinearElem (a,b,c,d) = Internal (a,b,c,d)
    linearPush a put = linearPush (desugar a) put
  -- TODO Larger tuples

instance (Linearizable m a, MonadComp m) => Linearizable m (Comp a)
  where
    type LinearElem (Comp a) = LinearElem a
    linearPush m put = liftComp m >>= flip linearPush put

instance Linearizable Run a => Linearizable Run (Run a)
  where
    type LinearElem (Run a) = LinearElem a
    linearPush m put = m >>= flip linearPush put

instance (Type a, MonadComp m) => Linearizable m (Dim1 (IArr a))
  where
    type LinearElem (Dim1 (IArr a)) = a
    linearPush (Dim1 len arr) put =
        for (0,1,Excl len) $ \i ->
          put (arr!i)

instance (Type a, MonadComp m) => Linearizable m (Dim1 (Arr a))
  where
    type LinearElem (Dim1 (Arr a)) = a
    linearPush (Dim1 len arr) put = do
        iarr <- unsafeFreezeArr arr
        linearPush (Dim1 len iarr) put

instance (Linearizable m a, MonadComp m) => Linearizable m (Pull a)
  where
    type LinearElem (Pull a) = LinearElem a
    linearPush (Pull len ixf) put =
        for (0,1,Excl len) $ \i ->
          linearPush (ixf i) put

instance (Linearizable m a, MonadComp m) => Linearizable m (PushSeq a)
  where
    type LinearElem (PushSeq a) = LinearElem a
    linearPush (PushSeq dump) = dump . flip linearPush

-- | Fold the content of a 'Linearizable' data structure
linearFold :: (Linearizable Comp lin, Syntax a) =>
    (a -> Data (LinearElem lin) -> a) -> a -> lin -> a
linearFold step init lin = unsafePerform $ do
    let dump = linearPush lin
    r <- initRef init
    let put = modifyRef r . flip step
    dump put
    unsafeFreezeRef r

-- | Fold the content of a 'Linearizable' data structure
linearFoldM :: (Linearizable m lin, Syntax a, MonadComp m) =>
    (a -> Data (LinearElem lin) -> m a) -> a -> lin -> m a
linearFoldM step init lin = do
    let dump = linearPush lin
    r <- initRef init
    let put a = do
          s <- unsafeFreezeRef r
          setRef r =<< step s a
    dump put
    unsafeFreezeRef r

-- | Write the content of a 'Linearizable' data structure to an array
linearArr :: (Linearizable m a, Type (LinearElem a), MonadComp m)
    => Arr (LinearElem a)  -- ^ Where to put the result
    -> a                   -- ^ Value to linearize
    -> m (Data Length)     -- ^ Number of elements written
linearArr arr a = do
    r <- initRef (0 :: Data Index)
    let put b = do
          i <- unsafeFreezeRef r
          setArr i b arr
          setRef r (i+1)
    linearPush a put
    unsafeFreezeRef r



--------------------------------------------------------------------------------
-- * Examples
--------------------------------------------------------------------------------

-- | The span of a vector (difference between greatest and smallest element)
spanVec :: Pull (Data Float) -> Data Float
spanVec vec = hi-lo
  where
    (lo,hi) = fold (\(l,h) a -> (min a l, max a h)) (vec!0,vec!0) vec
  -- This demonstrates how tuples interplay with sharing. Tuples are essentially
  -- useless without sharing. This function would get two identical for loops if
  -- it wasn't for sharing.

-- | Apply a function to chunks of a @Pull@ vector
chunked :: (Syntax b, MonadComp m)
        => Int                 -- ^ Size of the chunks
        -> (Pull a -> Pull b)  -- ^ Applied to every chunk
        -> Pull a
        -> m (Dim1 (IArr (Internal b)))
chunked c f vec = do
  let c' = fromInteger $ toInteger c
      len = length vec
      (noc,l2) = len `quotRem` c'
      l1  = c' * noc
  arr <- newArr len
  off <- initRef (0 :: Data Index)
  for (0,1,Excl noc) $ \i -> do
    let v = f $ take c' $ drop i vec
    for (0,1,Excl c') $ \j -> do
      x <- unsafeFreezeRef off
      setArr x (v!j) arr
      setRef off (x+1)
  let v = f $ take c' $ drop l1 vec
  for (0,1,Excl l2) $ \i -> do
    x <- unsafeFreezeRef off
    setArr x (v!i) arr
    setRef off (x+1)
  iarr <- unsafeFreezeArr arr
  return $ Dim1 len iarr

