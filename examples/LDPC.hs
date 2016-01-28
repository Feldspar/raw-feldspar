module LDPC where

import Feldspar

--------------------------------------------------------------------------------
-- * Matrices.
--------------------------------------------------------------------------------

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
-- ** Find an LU decomposition of a sparse matrix.

decompose
  :: Mat a   -- ^ Input matrix, M by N.
  -> Int     -- ^ K
  -> Int     -- ^ Size of sub-matrix to find LU decomposition of
  -> Int     -- ^ Number of columns to abandom.
  -> Int     -- ^ Whent to abandom said columns.
  -> ( Mat a -- ^ L matrix, M by K.
     , Mat a -- ^ U matrix, K by N.
     )
decompose = undefined

--------------------------------------------------------------------------------
-- * Encoding.
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
