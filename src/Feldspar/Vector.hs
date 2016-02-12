module Feldspar.Vector where



import Prelude ()

import Feldspar



freezeVec :: (MonadComp m, Type a) =>
    Data Length -> Arr a -> m (Vector (Data a))
freezeVec len arr = do
    iarr <- freezeArr arr len
    return $ Indexed len $ \i -> arrIx iarr i

unsafeFreezeVec :: (MonadComp m, Type a) =>
    Data Length -> Arr a -> m (Vector (Data a))
unsafeFreezeVec len arr = do
    iarr <- unsafeFreezeArr arr
    return $ Indexed len $ \i -> arrIx iarr i

data Vector a
  where
    Indexed :: Data Length -> (Data Index -> a) -> Vector a

instance Type a => Storable (Vector (Data a))
  where
    type StoreRep (Vector (Data a)) = (Ref Length, Arr a)
    initStoreRep vec = do
        arr <- newArr len
        lenRef <- initRef len
        writeStoreRep (lenRef,arr) vec
        return (lenRef,arr)
      where
        len = length vec
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
tail (Indexed l ixf) = Indexed (l-1) (ixf . (+1))

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
l ... h = Indexed (h-l+1) (+l)

infix 3 ...

map :: (a -> b) -> Vector a -> Vector b
map f (Indexed len ixf) = Indexed len (f . ixf)

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f a b = map (uncurry f) $ zip a b

fold :: Syntax b => (a -> b -> b) -> b -> Vector a -> b
fold f b (Indexed len ixf) = forLoop len b (\i st -> f (ixf i) st)

fold1 :: Syntax a => (a -> a -> a) -> Vector a -> a
fold1 f (Indexed len ixf) = forLoop len (ixf 0) (\i st -> f (ixf i) st)

sum :: (Num a, Syntax a) => Vector a -> a
sum = fold (+) 0

type Matrix a = Vector (Vector (Data a))

-- | Transpose of a matrix. Assumes that the number of rows is > 0.
transpose :: Type a => Matrix a -> Matrix a
transpose a = Indexed (length (a!0)) $ \k -> Indexed (length a) $ \l -> a ! l ! k



--------------------------------------------------------------------------------
-- * Examples
--------------------------------------------------------------------------------

-- | The span of a vector (difference between greatest and smallest element)
spanVec :: Vector (Data Float) -> Data Float
spanVec vec = hi-lo
  where
    (lo,hi) = fold (\a (l,h) -> (min a l, max a h)) (vec!0,vec!0) vec
  -- This demonstrates how tuples interplay with sharing. Tuples are essentially
  -- useless without sharing. This function would get two identical for loops if
  -- it wasn't for sharing.

-- | Scalar product
scProd :: Vector (Data Float) -> Vector (Data Float) -> Data Float
scProd a b = sum (zipWith (*) a b)

forEach = flip map

-- | Matrix multiplication
matMul :: Matrix Float -> Matrix Float -> Matrix Float
matMul a b = forEach a $ \a' ->
               forEach (transpose b) $ \b' ->
                 scProd a' b'

