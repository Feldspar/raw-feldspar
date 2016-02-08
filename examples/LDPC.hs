module LDPC where

import Data.Word

import Feldspar hiding (when)
import Feldspar.Signature

import Prelude  hiding ((==), (>=), until, not, while, break)

--------------------------------------------------------------------------------
-- * Matrices.
--------------------------------------------------------------------------------
--
-- *** The matrices used in LDPC are typically sparse. Our model should reflect
--     this.

-- | Representation of matrices as a row & col count and indexing function.
data Matrix a = Matrix (Data Length) (Data Length) (Data Index -> Data Index -> a)

instance Functor Matrix
  where
    fmap f (Matrix rows cols ixf) = Matrix rows cols (\row col -> f $ ixf row col)

-- | Short-hand for matrices over Feldspar values.
type Mat    a = Matrix (Data a)

-- | Storable instance to declare memory representation of matrices.
instance Syntax a => Storable (Matrix a)
  where
    type StoreRep (Matrix a) = (Ref Length, Ref Length, Arr (Internal a))
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
         return $ fmap sugar $ unsafeFreezeMatrix r c array
    unsafeFreezeStoreRep (rows, cols, array) =
      do r <- unsafeFreezeRef rows
         c <- unsafeFreezeRef cols
         return $ fmap sugar $ unsafeFreezeMatrix r c array
    writeStoreRep (rows, cols, array) (Matrix r c ixf) =
      do offset <- unsafeFreezeRef cols
         for (0, 1, Excl r) $ \i ->
           for (0, 1, Excl c) $ \j ->
             setArr (i * offset + j) (desugar $ ixf i j) array
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
-- ** Matrix LDPC specific functions.

type Bit = Bool

high, low :: Data Bit
high = value True
low  = value False

xor :: Data Bit -> Data Bit -> Data Bit
xor x y = not x == y

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

-- | Multiply a mod2 matrix and array.
matrix_mul_vec :: MonadComp m => Mat Bit -> Arr Bit -> m (Arr Bit)
matrix_mul_vec mat@(Matrix rows cols ixf) arr = do
  out <- newArr rows
  upward rows $ \i ->
    setArr i low out
  upward cols $ \j ->
    when (arr `at` j) $
      matrix_cols mat j $ \x ->
        setArr x (out `at` x `xor` high) out
  return out

-- | Check if a codeword is correct according to some parity check matrix.
matrix_check :: MonadComp m => Mat Bit -> Arr Bit -> m (Data Bit)
matrix_check mat@(Matrix rows cols _) arr = do
  mul <- matrix_mul_vec mat arr
  chk <- initRef high
  upward rows $ \i ->
    when (mul `at` i) $
      setRef chk low
  unsafeFreezeRef chk

----------------------------------------

loop :: MonadComp m => m () -> m ()
loop = while (return high)

when :: MonadComp m => Data Bit -> m () -> m ()
when b m = iff b m (return ())

at :: Type a => Arr a -> Data Index -> Data a
at = unsafeArrIx

--------------------------------------------------------------------------------
-- * LDPC.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Encoding.

-- | Encode a message using a sparse generator matrix.
-- *** ToDo: LU decomposition of x and y, not sure if needed.
encode :: forall m. MonadComp m => Mat Bit -> Arr Index -> Arr Bit -> m (Arr Bit)
encode mat@(Matrix cols rows _) order msg = do
  code <- newArr rows :: m (Arr Bit)
  upward rows $ \i ->
    setArr i low x
  for (rows, 1, Excl cols) $ \j ->
    do v <- unsafeFreezeArr (j - rows) msg
       setArr (order `at` j) v code
  return code

--------------------------------------------------------------------------------
-- ** Decoding.

-- | ...
decode :: forall m. MonadComp m => Mat Bit -> Arr Double -> m (Arr Bit)
decode mat@(Matrix _ _ _) lr = do
  (guess, pmat) <- init_prp mat lr
  loop $ do
    c <- matrix_check mat guess
    when c
      break
    iter_prp mat pmat lr guess
  return guess

--------------------------------------------------------------------------------
-- *** Probability propagation.

-- | Matrix over likelihoods or probabilites.
type PMat = Matrix (Data Double, Data Double)
  
-- | Initialize probability propagation.
init_prp :: forall m. MonadComp m => Mat Bit -> Arr Double -> m (Arr Bit, PMat)
init_prp mat@(Matrix rows cols _) arr = do
  pr  <- newArr (cols * rows) :: m (Arr Double)
  lr  <- newArr (cols * rows) :: m (Arr Double)
  dec <- newArr cols          :: m (Arr Bit)

  -- Set initial pr and lr fields.
  upward cols $ \j ->
    do matrix_cols mat j $ \i ->
         do let ix = i * cols + j
            setArr ix (arr `at` j) pr
            setArr ix 1 lr
       setArr j (arr `at` j >= 1) dec
       
  -- Construct pr/lr matrix from pr and lr arrays.
  -- *** ToDo: express using zip.
  return (dec, Matrix rows cols $ \row col ->
               let ix = row * cols + col
                in (pr `at` ix, lr `at` ix))

-- | Perform one iteration of the probability propagation.
iter_prp :: forall m. MonadComp m => Mat Bit -> PMat -> Arr Double -> Arr Bit -> m ()
iter_prp mat@(Matrix rows cols _) pmat arr guess = do
  lmat@(_, _, parr) <- initStoreRep pmat

  -- Recompute likelihood ratios.
  upward rows $ \i ->
    do dl <- initRef (1 :: Data Double)
       matrix_rows mat i $ \j ->
         do let ix = i * cols + j
            let pr = getPR parr ix
            dlv <- unsafeFreezeRef dl
            setLR parr ix dlv
            modifyRef dl $ \v -> v * (0.5 * (1 + pr) - 1)
       setRef dl (1 :: Data Double)
       t <- newRef
       matrix_rows_rev mat i $ \j ->
         do let ix = i * cols + j
            let pr = getPR parr ix
            let lr = getLR parr ix
            dlv <- unsafeFreezeRef dl
            tv  <- unsafeFreezeRef t
            setRef t (lr * dlv)
            setLR parr ix $ (1 - tv) / (1 + tv)
            modifyRef dl $ \v -> v * (0.5 * (1 + pr) - 1)

  -- Recompute probability ratios and make guess.
  upward cols $ \j ->
    do prr <- initRef (arr `at` j)
       matrix_cols mat j $ \i ->
         do let ix = i * cols + j
            prv <- unsafeFreezeRef prr
            setPR  parr ix prv
            setRef prr (getLR parr ix)
       prob <- getRef prr
       setArr j (prob >= 1) guess      -- make educated guess.
       setRef prr (1 :: Data Double)
       matrix_cols_rev mat j $ \i ->
         do let ix = i * cols + j
            prv <- unsafeFreezeRef prr
            setPR  parr ix prv
            setRef prr (getLR parr ix)

  -- Write changes from lowered pr/lr matrix to original.
  -- *** ToDo: update using unsafe to avoid extra code.
  writeStoreRep lmat pmat

setLR :: MonadComp m => Arr (Double, Double) -> Data Index -> Data Double -> m ()
setLR parr ix lr = setArr ix (desugar (getPR parr ix, lr)) parr

setPR :: MonadComp m => Arr (Double, Double) -> Data Index -> Data Double -> m ()
setPR parr ix pr = setArr ix (desugar (pr, getLR parr ix)) parr

getLR :: Arr (Double, Double) -> Data Index -> Data Double
getLR parr ix = snd (sugar $ unsafeArrIx parr ix :: (Data Double, Data Double))
    
getPR :: Arr (Double, Double) -> Data Index -> Data Double
getPR parr ix = fst (sugar $ unsafeArrIx parr ix :: (Data Double, Data Double))

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** ...

-- | Pass paramaters as one would in C.
encode_wrap
  :: forall m. MonadComp m
  => Arr Bit     -- Matrix contents with
  -> Ref Length  -- M rows and
  -> Ref Length  -- N columns.
  -> Arr Index   -- Ordering of generator matrix.
  -> Arr Bit     -- Source message.
  -> Arr Bit     -- Destination for codeword.
  -> m ()
encode_wrap marr row col order msg code =
  do mat <- readStoreRep (row, col, marr) :: m (Mat Bit)
     new <- encode mat order msg
     len <- getRef row
     copyArr new code len

encode_sig
  :: MonadComp m => Signature m (Arr Bit -> Ref Length -> Ref Length -> Arr Index -> Arr Bit -> Arr Bit -> m ())
encode_sig =
  lama $ \ar   ->
  lamr $ \rr   ->
  lamr $ \rc   ->
  lama $ \o    ->
  lama $ \msg  ->
  lama $ \code ->
  unit $ encode_wrap ar rr rc o msg code

--------------------------------------------------------------------------------
-- ** ...

decode_wrap
  :: forall m. MonadComp m
  => Arr Bit
  -> Ref Length
  -> Ref Length
  -> Arr Double
  -> Arr Bit
  -> m ()
decode_wrap marr row col pr msg =
  do mat <- readStoreRep (row, col, marr) :: m (Mat Bit)
     new <- decode mat pr
     len <- getRef row
     copyArr new msg len

decode_sig
  :: MonadComp m => Signature m (Arr Bit -> Ref Length -> Ref Length -> Arr Double -> Arr Bit -> m ())
decode_sig =
  lama $ \ar  ->
  lamr $ \rr  ->
  lamr $ \rc  ->
  lama $ \pr  ->
  lama $ \msg -> 
  unit $ decode_wrap ar rr rc pr msg

--------------------------------------------------------------------------------
-- ** ...



--------------------------------------------------------------------------------
