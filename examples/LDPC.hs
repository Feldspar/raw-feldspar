module LDPC where

import Feldspar

import Prelude hiding ((==))

--------------------------------------------------------------------------------
-- * Matrices.
--------------------------------------------------------------------------------
--
-- *** The matrices used in LDPC are typically sparse. Our model should reflect
--     this.

-- | Representation of matrices as a row & col count and indexing function.
data Matrix a = Matrix (Data Index) (Data Index) (Data Index -> Data Index -> a)

-- | Short-hand for matrices over Feldspar expressions.
type Mat a = Matrix (Data a)

-- | Storable instance to declare memory representation of matrices.
instance Type a => Storable (Matrix (Data a))
  where
    type StoreRep (Matrix (Data a)) = (Ref Length, Ref Length, Arr a)
    initStoreRep m@(Matrix rows cols _) =
      do a <- newArr (rows * cols)
         r <- initRef rows
         c <- initRef cols
         let n = (r, c, a)
         writeStoreRep n m
         return n
    readStoreRep (rows, cols, array) =
      do r <- unsafeFreezeRef rows
         c <- unsafeFreezeRef cols
         return $ freezeMatrix r c array
    unsafeFreezeStoreRep (rows, cols, array) =
      do r <- unsafeFreezeRef rows
         c <- unsafeFreezeRef cols
         return $ freezeMatrix r c array
    writeStoreRep (rows, cols, array) (Matrix r c ixf) =
      do offset <- unsafeFreezeRef cols
         for (0, 1, Excl r) $ \i ->
           for (0, 1, Excl c) $ \j ->
             setArr (i * offset + j) (ixf i j) array
    copyStoreRep _ (rd, cd, dst) (rs, cs, src) =
      do rows <- unsafeFreezeRef rs
         cols <- unsafeFreezeRef cs
         setRef rd rows
         setRef cd cols
         copyArr dst src (rows * cols)

-- | Create a matrix of some row & col count 
freezeMatrix :: Type a => Data Length -> Data Length -> Arr a -> Matrix (Data a)
freezeMatrix rows cols array = Matrix rows cols $ \r c -> unsafeArrIx array (r * cols + c)

--------------------------------------------------------------------------------
-- ** Frontend stuff. 

matrix_get :: Data Index -> Data Index -> Matrix (Data a) -> Data a
matrix_get row col (Matrix _ _ ixf) = ixf row col

--------------------------------------------------------------------------------
-- * LDPC.
--------------------------------------------------------------------------------

type Array = Arr -- names
type Bit   = Bool

high, low :: Data Bit
high = value True
low  = value False

--------------------------------------------------------------------------------

check :: MonadComp m => Matrix (Data Bit) -> Array Bit -> m (Data Bool)
check m@(Matrix rows cols _) a = do
  mul <- matrix_mul_vec m a
  chk <- initRef low
  for (0, 1, Excl cols) $ \i ->
    undefined
  getRef chk

matrix_mul_vec :: forall m. MonadComp m => Matrix (Data Bit) -> Array Bit -> m (Array Bit)
matrix_mul_vec (Matrix rows cols ixf) vec = do
  out <- newArr cols :: m (Array Bit)
  for (0, 1, Excl cols) $ \i -> do
    iff (unsafeArrIx vec i)
      (do sum <- initRef low
          for (0, 1, Excl rows) $ \j -> 
            -- *** This should be an (^=), i.e. xor assignment.
            do iff (unsafeArrIx vec i == ixf j i)
                 (setRef sum high)
                 (setRef sum low)
            -- ***
          end <- unsafeFreezeRef sum
          setArr i end out)
      (do setArr i low out)
  return out

--------------------------------------------------------------------------------
-- ** Encoding.

-- | One iteration of probability propagation.
iterp :: ()
iterp = undefined
  
--------------------------------------------------------------------------------
-- ** Decoding.

-- ...

--------------------------------------------------------------------------------
