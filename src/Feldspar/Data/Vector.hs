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
-- is a `Pull` matrix whose rows are `Push`.
--
-- There are also some disadvantages to nesting:
--
--   * We can't have nested manifest vectors, so conversion to/from memory
--     arrays becomes a bit ad hoc (see the various `toPull*` functions).
--   * There is no guarantee that the inner vectors have the same length, so
--     functions operating on matrices have to assume this property.
--   * Similarly, it is generally not possible to get the length of the inner
--     vectors, so it sometimes has to be provided from the outside (see e.g.
--     `concatPush`).
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
  -- It might seem useful to also have the function:
  --
  --     sequenceVec2 :: MonadComp m => vec (m a) -> Push a
  --
  -- However, that would lead to a `Push` vector with embedded effects, which is
  -- not allowed.



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
    toValue   = toValue . toPush
    fromValue = toPull

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

instance
    ( MarshalHaskell (Internal a)
    , MarshalFeld (Data (Internal a))
    , Syntax a
    ) =>
      MarshalFeld (Pull a)
  where
    type HaskellRep (Pull a) = [Internal a]
    fromFeld = fromFeld <=< toValue
    toFeld   = fmap sugar . toPull <$> (toFeld :: Run (Dim1 (IArr (Internal a))))
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



----------------------------------------
-- ** Conversion
----------------------------------------

-- | Vectors that are 'Pull'-like
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
toPull :: (Pully vec (Data (Internal a)), Syntax a) => vec -> Pull a
toPull vec = fmap sugar $ Pull (length vec) (vec!)
  -- This function is more general than `fromValue` since it also handles e.g.
  -- `Dim2`.

-- | Convert a 2-dimensional indexed array to a nested 'Pull' vector
toPull2 :: (Indexed arr, Syntax a, IndexedElem arr ~ Data (Internal a)) =>
    Dim2 arr -> Pull (Pull a)
toPull2 (Dim2 r c arr) =
    Pull r $ \k ->
      Pull c $ \l ->
        sugar (arr ! (k*c+l))

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

unzipWith :: Pully vec ab => (ab -> (a, b)) -> vec -> (Pull a, Pull b)
unzipWith f ab = (Pull len (fst . f . (ab!)), Pull len (snd . f . (ab!)))
  where
    len = length ab

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

-- | Transpose of a matrix. Assumes that the number of rows is > 0 and that all
-- rows have the same length.
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
--
-- The function that dumps the content of the vector is not allowed to perform
-- any side effects except through the \"write\" method
-- (@`Data` `Index` -> a -> m ()@) that is passed to it.
data Push a
  where
    Push
        :: Data Length
        -> (forall m . MonadComp m => (Data Index -> a -> m ()) -> m ())
        -> Push a
  -- The no-side-effects condition ensures that `Push` behaves as pure data with
  -- the denotation of a finite list.
  --
  -- TODO If there was an appropriate `store` function, it should be possible to
  -- formulate a law that `v :: Push a` should be equivalent to `store v`. If
  -- `v` has embedded effects, then e.g. `v++v` is not equivalent to
  -- `store v ++ store v`.

-- | 'Push' vector specialized to 'Data' elements
type DPush a = Push (Data a)

instance Functor Push
  where
    fmap f (Push len dump) = Push len $ \write ->
        dump $ \i -> write i . f

instance Finite (Push a)
  where
    length (Push len _) = len

instance Syntax a => Forcible (Push a)
  where
    type ValueRep (Push a) = Dim1 (IArr (Internal a))
    toValue (Push len dump) = do
        arr <- newArr len
        dump $ \i a -> setArr i a arr
        iarr <- unsafeFreezeArr arr
        return $ Dim1 len iarr
    fromValue = toPush . (id :: Pull b -> Pull b) . fromValue

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
          dump1 write >> dump2 (write . (+len1))

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
--
-- The function that dumps the content of the vector is not allowed to perform
-- any side effects except through the \"put\" method (@a -> m ()@) that is
-- passed to it.
data PushSeq a
  where
    PushSeq
        :: { dumpSeq :: forall m . MonadComp m => (a -> m ()) -> m () }
        -> PushSeq a
  -- The no-side-effects condition ensures that `PushSeq` behaves as pure data
  -- with the denotation of a (possibly infinite) list. It also ensures that the
  -- `Folding` instance is sound.
  --
  -- TODO If there was an appropriate `store` function, it should be possible to
  -- formulate a law that `v :: PushSeq a` should be equivalent to `store v`. If
  -- `v` has embedded effects, then e.g. `v++v` is not equivalent to
  -- `store v ++ store v`.

-- | 'PushSeq' vector specialized to 'Data' elements
type DPushSeq a = PushSeq (Data a)

instance Functor PushSeq
  where
    fmap f (PushSeq dump) = PushSeq $ \put -> dump (put . f)

-- No instance `Syntax a => Forcible (PushSeq a)` because `PushSeq` doesn't have
-- known length.

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
    fold step init = unsafePerform . foldM (\s -> return . step s) init
    foldM step init vec = do
        r <- initRef init
        let put a = do
              s <- unsafeFreezeRef r
              setRef r =<< step s a
        dumpSeq vec put
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
    dumpSeq (toPushSeq v1) put >> dumpSeq (toPushSeq v2) put

-- Concatenate nested vectors to a 'PushSeq' vector
concat :: (PushySeq vec1, PushySeq vec2, Functor vec1) =>
    vec1 (vec2 a) -> PushSeq a
concat vec =
    let PushSeq dump1 = toPushSeq $ fmap toPushSeq vec
    in  PushSeq $ \put ->
          dump1 $ \(PushSeq dump2) ->
            dump2 put

-- | Flatten a vector of elements with a static structure
flatten
    :: PushySeq vec
    => (a -> [b])  -- ^ Convert source element to a list of destination elements
    -> vec a
    -> PushSeq b
flatten f = concat . fmap (listPushSeq . f) . toPushSeq

filter :: PushySeq vec => (a -> Data Bool) -> vec a -> PushSeq a
filter pred v = PushSeq $ \put -> do
    let put' a = iff (pred a) (put a) (return ())
    dumpSeq (toPushSeq v) put'

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
             -- Note:
             --
             -- 1. This is `PushSeq` with `m` exposed. It needs to be exposed
             --    for the instances for monads to work.
             -- 2. We don't want to use `PushSeq` here (even if `m` was
             --    exposed), because linearizing structures with embedded
             --    effects would lead to a `PushSeq` with embedded effects,
             --    which is not a valid `PushSeq`.

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
    linearPush = linearPush . toPushSeq

instance (Linearizable m a, MonadComp m) => Linearizable m (PushSeq a)
  where
    type LinearElem (PushSeq a) = LinearElem a
    linearPush (PushSeq dump) = dump . flip linearPush

-- | Fold the content of a 'Linearizable' data structure
linearFold :: (Linearizable m lin, Syntax a, MonadComp m) =>
    (a -> Data (LinearElem lin) -> m a) -> a -> lin -> m a
linearFold step init lin = do
    let dump = linearPush lin
    r <- initRef init
    let put a = do
          s <- unsafeFreezeRef r
          setRef r =<< step s a
    dump put
    unsafeFreezeRef r

-- | Map a monadic function over the content of a 'Linearizable' data structure
linearMapM :: (Linearizable m lin, MonadComp m) =>
    (Data (LinearElem lin) -> m ()) -> lin -> m ()
linearMapM f = linearFold (\_ -> f) ()

-- | Write the content of a 'Linearizable' data structure to an array
linearWriteArr :: (Linearizable m a, Type (LinearElem a), MonadComp m)
    => Arr (LinearElem a)  -- ^ Where to put the result
    -> a                   -- ^ Value to linearize
    -> m (Data Length)     -- ^ Number of elements written
linearWriteArr arr a = do
    linearFold (\i b -> setArr i b arr >> return (i+1)) 0 a



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

