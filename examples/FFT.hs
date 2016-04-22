{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module FFT
  ( fft
  , ifft
  ) where



import Prelude ()

import Feldspar.Run
import Feldspar.Vector



rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

riffle :: Syntax a => Data Index -> Vector a -> Vector a
riffle = permute . const . rotBit

stages :: (Storable s, MonadComp m) => Vector a -> (a -> s -> s) -> s -> m s
stages as body init = do
    s <- initStore init
    for (0, 1, Excl (length as)) $ \i ->
        writeStore s . body (as!i) =<< readStore s
    unsafeFreezeStore s

bitRev :: (Type a, MonadComp m) =>
    Data Index -> Vector (Data a) -> m (Vector (Data a))
bitRev n = stages (1...n) riffle

testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0

fftCore :: MonadComp m
    => Data Index
    -> Vector (Data (Complex Double))
    -> m (Vector (Data (Complex Double)))
fftCore n = stages (reverse (0...n)) step
  where
    step k vec = Indexed (length vec) ixf
      where
        ixf i = testBit i k ? (twid * (b - a)) $ (a+b)
          where
            k'   = i2n k
            a    = vec ! i
            b    = vec ! (i `xor` k2)
            twid = polar 1 (-π * i2n (lsbs k' i) / i2n k2)
            k2   = 1 .<<. k'

-- | Radix-2 Decimation-In-Frequeny Fast Fourier Transformation of the given
-- complex vector. The given vector must be power-of-two sized, (for example 2,
-- 4, 8, 16, 32, etc.)
fft :: MonadComp m =>
    Vector (Data (Complex Double)) -> m (Vector (Data (Complex Double)))
fft v = do
    n' <- force n
    fftCore n' v >>= bitRev n'
  where
    n = ilog2 (length v) - 1

ifftCore :: MonadComp m
    => Data Index
    -> Vector (Data (Complex Double))
    -> m (Vector (Data (Complex Double)))
ifftCore n = stages (reverse (0...n)) step
  where
    step k vec = Indexed (length vec) ixf
      where
        ixf i = testBit i k ? (twid * (b - a)) $ (a+b)
          where
            k'   = i2n k
            a    = vec ! i
            b    = vec ! (i `xor` k2)
            twid = polar 1 (π * i2n (lsbs k' i) / i2n k2)
            k2   = 1 .<<. k'

-- | Radix-2 Decimation-In-Frequeny Inverse Fast Fourier Transformation of the
-- given complex vector. The given vector must be power-of-two sized, (for
-- example 2, 4, 8, 16, 32, etc.)
ifft :: MonadComp m =>
    Vector (Data (Complex Double)) -> m (Vector (Data (Complex Double)))
ifft v = ifftCore n v >>= bitRev n
  where
    n = ilog2 (length v) - 1

prog :: Run ()
prog = do
    n  <- fget stdin
--     let n = 31
    v  <- force $ map i2n (0...n)
    v' <- fft v
    printf "%.10f\n" $ sum $ map abs $ map realPart v'

