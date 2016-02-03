module LDPC where

import Data.Word

import Feldspar hiding (when)

import Prelude  hiding ((==), until)

--------------------------------------------------------------------------------
-- * Matrices.
--------------------------------------------------------------------------------
--
-- *** The matrices used in LDPC are typically sparse. Our model should reflect
--     this.

-- | Representation of matrices as a row & col count and indexing function.
data Matrix a = Matrix (Data Length) (Data Length) (Data Index -> Data Index -> a)

-- | Short-hand for matrices over Feldspar values.
type Mat    a = Matrix (Data a)

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
         return $ unsafeFreezeMatrix r c array
    unsafeFreezeStoreRep (rows, cols, array) =
      do r <- unsafeFreezeRef rows
         c <- unsafeFreezeRef cols
         return $ unsafeFreezeMatrix r c array
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
unsafeFreezeMatrix :: Type a => Data Length -> Data Length -> Arr a -> Matrix (Data a)
unsafeFreezeMatrix rows cols array = Matrix rows cols $ \r c -> unsafeArrIx array (r * cols + c)

--------------------------------------------------------------------------------
-- ** Matrix frontend.

-- ...

--------------------------------------------------------------------------------
-- ** Matrix LDPC specific functions.

type Bit = Bool

high, low :: Data Bit
high = value True
low  = value False

-- *** should be a primary.
xor :: Data Bit -> Data Bit -> Data Bit
xor = error "LDPC: missing xor"

----------------------------------------

upward :: MonadComp m => Data Length -> (Data Index -> m ()) -> m ()
upward l = for (0, 1, Excl l)

downward :: MonadComp m => Data Length -> (Data Index -> m ()) -> m ()
downward l f = upward l (f . ((l - 1) -))

----------------------------------------

matrix_rows :: MonadComp m => Mat Bit -> Data Index -> (Data Index -> m ()) -> m ()
matrix_rows (Matrix rows cols ixf) row f =
  upward cols $ \j -> iff (ixf row j) (f j) (return ())

matrix_rows_rev :: MonadComp m => Mat Bit -> Data Index -> (Data Index -> m ()) -> m ()
matrix_rows_rev (Matrix _ cols ixf) row f =
  downward cols $ \j -> iff (ixf row j) (f j) (return ())

matrix_cols :: MonadComp m => Mat Bit -> Data Index -> (Data Index -> m ()) -> m ()
matrix_cols (Matrix rows _ ixf) col f =
  upward rows $ \i -> iff (ixf i col) (f i) (return ())

matrix_cols_rev :: MonadComp m => Mat Bit -> Data Index -> (Data Index -> m ()) -> m ()
matrix_cols_rev (Matrix rows _ ixf) col f =
  downward rows $ \i -> iff (ixf i col) (f i) (return ())

----------------------------------------

matrix_mul_vec :: forall m. MonadComp m => Mat Bit -> Arr Bit -> m (Arr Bit)
matrix_mul_vec m@(Matrix rows cols ixf) arr = do
  out <- newArr rows :: m (Arr Bit)
  upward rows $ \i ->
    setArr i low out
  upward cols $ \j ->
    when (arr `at` j) $
      matrix_cols m j $ \x ->
        setArr x (out `at` x `xor` high) out
  return out

----------------------------------------

when :: MonadComp m => Data Bit -> m () -> m ()
when b m = iff b m (return ())

at :: Type a => Arr a -> Data Index -> Data a
at = unsafeArrIx

--------------------------------------------------------------------------------
-- * LDPC.
--------------------------------------------------------------------------------
{-
-- | Multiply a mod2 matrix and vector.
matrix_mul_vec :: forall m. MonadComp m => Matrix (Data Bit) -> Array Bit -> m (Array Bit)
matrix_mul_vec (Matrix rows cols ixf) vec = do
  out <- newArr cols :: m (Array Bit)
  for (0, 1, Excl cols) $ \i -> do
    iff (unsafeArrIx vec i)
      (do sum <- initRef low
          for (0, 1, Excl rows) $ \j -> 
            -- *** This should be an (^=), i.e. xor assignment.
            do iff (unsafeArrIx vec i == ixf j i)
                 (setRef sum low)
                 (setRef sum high)
            -- ***
          end <- unsafeFreezeRef sum
          setArr i end out)
      (do setArr i low out)
  return out
-}
--------------------------------------------------------------------------------
-- ** Encoding.
{-
-- | Matrix over likelihoods or probabilites.
type PMat = Matrix (Data Double)

-- | One iteration of probability propagation.
iterp :: ()
iterp = undefined

-- | Initialize probability propagation.
init_prp :: MonadComp m => Matrix (Data Bit) -> Array Double -> m (PMat, PMat)
init_prp m@(Matrix rows cols _) a = do
  let lr = newMat rows cols (\_ _ -> 1 :: Data Double)
  let pr = newMat rows cols (\_ c -> unsafeArrIx a c)
  return (lr, pr)

-- | Perform one step of the probability propagation.
iter_prp :: MonadComp m => Matrix (Data Bit) -> (PMat, PMat) -> Array Double -> m (Array Double)
iter_prp m@(Matrix rows cols _) ((Matrix _ _ ixLr), (Matrix _ _ ixPr)) a = do
  -- Recompute likelihood ratios.
  for (0, 1, Excl rows) $ \i ->
    do dl <- initRef (0 :: Data Double)
       for (0, 1, Excl cols) $ \j ->
         undefined
  -- Recompute probability ratios and guess.
  undefined

-- | Check number of incorrect bits in a codeword.
check :: MonadComp m => Matrix (Data Bit) -> Array Bit -> m (Data Word16)
check m@(Matrix rows cols _) a = do
  mul <- matrix_mul_vec m a
  chk <- initRef (0 :: Data Word16)
  for (0, 1, Excl cols) $ \i ->
    do iff (unsafeArrIx mul i)
         (do x <- unsafeFreezeRef chk
             setRef chk (x + 1))
         (return ())
  getRef chk
-}
--------------------------------------------------------------------------------
-- ** Decoding.

-- ...

--------------------------------------------------------------------------------
