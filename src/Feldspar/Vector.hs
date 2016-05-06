module Feldspar.Vector where



import Prelude ()
import Data.Proxy

import Feldspar
import Feldspar.Run
import Feldspar.Run.Concurrent
import qualified Language.Embedded.Concurrent as Imp



freezeVec :: (MonadComp m, Syntax a) =>
    Data Length -> Arr (Internal a) -> m (Vector a)
freezeVec len arr = do
    iarr <- freezeArr arr len
    return $ Indexed len $ \i -> arrIx iarr i

unsafeFreezeVec :: (MonadComp m, Syntax a) =>
    Data Length -> Arr (Internal a) -> m (Vector a)
unsafeFreezeVec len arr = do
    iarr <- unsafeFreezeArr arr
    return $ Indexed len $ \i -> arrIx iarr i

data Vector a
  where
    Indexed :: Data Length -> (Data Index -> a) -> Vector a

instance Syntax a => Forcible (Vector a)
  where
    type ValueRep (Vector a) = Manifest a
    toValue   = fromPull
    fromValue = toPull

instance Syntax a => Storable (Vector a)
  where
    type StoreRep (Vector a)  = (Ref Length, Arr (Internal a))
    type StoreSize (Vector a) = Data Length
    newStoreRep _ len = do
        lenRef <- initRef len
        arr    <- newArr len
        return (lenRef,arr)
    initStoreRep vec = do
        rep <- newStoreRep (Nothing :: Maybe (Vector a)) (length vec)
        writeStoreRep rep vec
        return rep
    readStoreRep (lenRef,arr) = do
        len <- getRef lenRef
        freezeVec len arr
    unsafeFreezeStoreRep (lenRef,arr) = do
        len <- unsafeFreezeRef lenRef
        unsafeFreezeVec len arr
    writeStoreRep (lenRef,arr) (Indexed l ixf) =
        for (0, 1, Excl l) $ \i -> setArr i (ixf i) arr
    copyStoreRep _ (dLenRef,dst) (sLenRef,src) = do
        sLen <- unsafeFreezeRef sLenRef
        setRef dLenRef sLen
        copyArr dst src sLen

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) =>
    MarshalFeld (Vector (Data a))
  where
    type HaskellRep (Vector (Data a)) = [a]

    fromFeld vec = do
        Manifest l arr <- fromPull vec
        fromFeld $ OfLength l arr

    toFeld = do
        OfLength l arr <- toFeld
        return $ toPull $ Manifest l arr

data VecChanSizeSpec lenSpec = VecChanSizeSpec (Data Length) lenSpec

ofLength :: Data Length -> lenSpec -> VecChanSizeSpec lenSpec
ofLength = VecChanSizeSpec

instance ( Syntax a, BulkTransferable a
         , ContainerType a ~ Arr (Internal a)
         ) => Transferable (Vector a)
  where
    type SizeSpec (Vector a) = VecChanSizeSpec (SizeSpec a)
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


length :: Vector a -> Data Length
length (Indexed len _) = len

index :: Vector a -> Data Index -> a
index (Indexed _ ixf) = ixf

(!) :: Vector a -> Data Index -> a
Indexed _ ixf ! i = ixf i

infixl 9 !

head :: Vector a -> a
head = (!0)

tail :: Vector a -> Vector a
tail = drop 1

take :: Data Length -> Vector a -> Vector a
take l (Indexed m f) = Indexed (min m l) f

drop :: Data Length -> Vector a -> Vector a
drop l (Indexed m f) = Indexed (b2i (l<=m) * (m-l)) (f . (+l))

splitAt :: Data Index -> Vector a -> (Vector a, Vector a)
splitAt l vec = (take l vec, drop l vec)

tails :: Vector a -> Vector (Vector a)
tails vec = Indexed (length vec + 1) (`drop` vec)

inits :: Vector a -> Vector (Vector a)
inits vec = Indexed (length vec + 1) (`take` vec)

inits1 :: Vector a -> Vector (Vector a)
inits1 = tail . inits

replicate :: Data Length -> a -> Vector a
replicate l = Indexed l . const

zip :: Vector a -> Vector b -> Vector (a,b)
zip a b = Indexed (length a `min` length b) (\i -> (index a i, index b i))

unzip :: Vector (a,b) -> (Vector a, Vector b)
unzip ab = (Indexed len (fst . index ab), Indexed len (snd . index ab))
  where
    len = length ab

permute :: (Data Length -> Data Index -> Data Index) -> (Vector a -> Vector a)
permute perm vec = Indexed len (index vec . perm len)
  where
    len = length vec

reverse :: Vector a -> Vector a
reverse = permute $ \len i -> len-i-1

(...) :: Data Index -> Data Index -> Vector (Data Index)
l ... h = Indexed (b2i (l<h+1) * (h-l+1)) (+l)

infix 3 ...

map :: (a -> b) -> Vector a -> Vector b
map f (Indexed len ixf) = Indexed len (f . ixf)

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f a b = map (uncurry f) $ zip a b

fold :: Syntax a => (a -> b -> a) -> a -> Vector b -> a
fold f x (Indexed l ixf) = forLoop l x $ \i s -> f s (ixf i)

fold1 :: Syntax a => (a -> a -> a) -> Vector a -> a
fold1 f (Indexed len ixf) = forLoop len (ixf 0) (\i st -> f (ixf i) st)

sum :: (Num a, Syntax a) => Vector a -> a
sum = fold (+) 0

type Matrix a = Vector (Vector (Data a))

indexedMat
    :: Data Length  -- ^ # rows
    -> Data Length  -- ^ # columns
    -> (Data Index -> Data Index -> Data a)
         -- ^ @row index -> column index -> element@
    -> Matrix a
indexedMat r c ixf = Indexed r $ \k -> Indexed c $ \l -> ixf k l

-- | Transpose of a matrix. Assumes that the number of rows is > 0.
transpose :: Type a => Matrix a -> Matrix a
transpose a = Indexed (length (a!0)) $ \k -> Indexed (length a) $ \l -> a ! l ! k



--------------------------------------------------------------------------------
-- * Manifest vectors
--------------------------------------------------------------------------------

data Manifest a = Manifest (Data Length) (IArr (Internal a))

toPull :: Syntax a => Manifest a -> Vector a
toPull (Manifest len arr) = Indexed len (arrIx arr)

fromPull :: (Syntax a, MonadComp m) => Vector a -> m (Manifest a)
fromPull (Indexed len ixf) = do
    arr <- newArr len
    for (0,1,Excl len) $ \i -> setArr i (ixf i) arr
    iarr <- unsafeFreezeArr arr
    return $ Manifest len iarr



--------------------------------------------------------------------------------
-- * Examples
--------------------------------------------------------------------------------

-- | The span of a vector (difference between greatest and smallest element)
spanVec :: Vector (Data Float) -> Data Float
spanVec vec = hi-lo
  where
    (lo,hi) = fold (\(l,h) a -> (min a l, max a h)) (vec!0,vec!0) vec
  -- This demonstrates how tuples interplay with sharing. Tuples are essentially
  -- useless without sharing. This function would get two identical for loops if
  -- it wasn't for sharing.

-- | Scalar product
scProd :: (Num a, Syntax a) => Vector a -> Vector a -> a
scProd a b = sum (zipWith (*) a b)

forEach = flip map

-- | Matrix multiplication
matMul :: (Num a, PrimType a) => Matrix a -> Matrix a -> Matrix a
matMul a b = forEach a $ \a' ->
               forEach (transpose b) $ \b' ->
                 scProd a' b'

-- | Apply a function to chunks of a @Vector@
chunked :: (Syntax b, MonadComp m)
        => Int                         -- ^ Size of the chunks
        -> (Vector a -> Vector b)      -- ^ Applied to every chunk
        -> Vector a
        -> m (Manifest b)
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
  return $ Manifest len iarr
