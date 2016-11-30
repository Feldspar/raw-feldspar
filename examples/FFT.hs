{-# LANGUAGE FlexibleContexts #-}

-- | FFT implementation inspired by the paper "Feldspar: Application and
-- implementation":
--
-- <http://publications.lib.chalmers.se/records/fulltext/local_156271.pdf>
--
-- There are a few differences, partly due to the paper using a different
-- Feldspar implementation. But regardless, the best way to understand the
-- definitions in this file is by reading the paper.

module FFT
  ( tw  -- Exported to allow pre-computation
  , fftCore
  , fft
  , ifft
  ) where



import Prelude ()

import Data.Bool (bool)

import Feldspar.Run
import Feldspar.Data.Vector
import Feldspar.Data.Buffered



----------------------------------------
-- * Helper functions
----------------------------------------

rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

riffle :: (Pully pull a, Syntax a) => Data Index -> pull -> Pull a
riffle = backPermute . const . rotBit

testBit :: (Bits a, Integral a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = i2b (a .&. (1 .<<. i2n i))

-- | @2^n@
twoTo :: (Num a, Bits a, PrimType a) => Data Index -> Data a
twoTo n = 1 .<<. i2n n

flipBit :: (Num a, Bits a, PrimType a) => Data a -> Data Index -> Data a
flipBit i k = i `xor` twoTo k



----------------------------------------
-- * Bit-reversal permutation
----------------------------------------

bitRev :: (Manifestable Run vec a, Finite vec, Syntax a)
    => Store a
    -> Length  -- ^ Unrolling steps in inner loops (1 means no unrolling)
    -> Data Length
    -> vec
    -> Run (Manifest a)
bitRev st u n = loopStore st (1,1,Excl n) $ \i -> return . unroll u . riffle i



----------------------------------------
-- * FFT
----------------------------------------

tw :: (Floating a, PrimType a, PrimType (Complex a))
    => Bool  -- ^ Inverse FFT?
    -> Data Index
    -> Data Index
    -> Data (Complex a)
tw inv n k = polar 1 (bool (-2) 2 inv * π * i2n k / i2n n)

twids
    :: ( Pully ts (Data (Complex a))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       , Pully vec (Data (Complex a))
       )
    => ts
    -> Data Index
    -> Data Index
    -> Data Length
    -> vec
    -> DPull (Complex a)
twids ts n k l vec = Pull l $ \i ->
    let j = (lsbs (i2n k) i) .<<. (n'-1-k')
    in  (testBit i k) ? ((ts!j) * (vec!i)) $ (vec!i)
  where
    n' = i2n n
    k' = i2n k

bfly
    :: ( Pully vec (Data (Complex a))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       )
    => Data Index -> vec -> DPull (Complex a)
bfly k as = Pull (length as) $ \i ->
    let a = as ! i
        b = as ! flipBit i k
    in  (testBit i k) ? (b-a) $ (a+b)

-- | Core of the FFT
--
-- It is normally better to use 'fft' or 'ifft' than this functon; however, for
-- doing repeated FFT on vectors of the same size, 'fftCore' can be used to
-- avoid recomputing the twiddle factors and the number of stages.
fftCore
    :: ( Pully ts (Data (Complex a))
       , Manifestable Run vec (Data (Complex a))
       , Finite vec
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       )
    => Store (Data (Complex a))
    -> Length       -- ^ Unrolling steps in inner loops (1 means no unrolling)
    -> ts           -- ^ Twiddle factors
    -> Data Length  -- ^ Number of stages
    -> vec
    -> Run (DManifest (Complex a))
fftCore st u ts n vec = do
    let step i = return . unroll u . twids ts n i (length vec) . bfly i
    loopStore st ((i2n n :: Data Int32)-1,-1,Incl 0) (step . i2n) vec
      >>= bitRev st u n
      -- `i2n` is used to make the loop index a signed number. Otherwise the
      -- index will wrap to maxBound before the loop test after the final
      -- iteration.
      --
      -- An alternative is to use:
      --
      --     loopStore st (n,-1,Excl 0) (step . subtract 1) vec

-- | Radix-2 Decimation-In-Frequency Fast Fourier Transformation of the given
-- complex vector. The given vector must be power-of-two sized, (for example 2,
-- 4, 8, 16, 32, etc.) The output is non-normalized.
--
-- The length of the vector must be divisible by the number of unrolling steps.
--
-- The optimal amount of unrolling depends on the target architecture, but a
-- value of 2 might be a reasonable default that gives some performance
-- improvements on many systems and doesn't lead to too much code size increase.
fft
    :: ( Manifestable Run vec (Data (Complex a))
       , Finite vec
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       )
    => Store (Data (Complex a))
    -> Length  -- ^ Unrolling steps in inner loops (1 means no unrolling)
    -> vec
    -> Run (DManifest (Complex a))
fft st u vec = do
    n  <- shareM (ilog2 (length vec))
    ts <- manifestFresh $ Pull (twoTo (n-1)) (tw False (twoTo n))
      -- Change `manifestFresh` to `return` to avoid pre-computing twiddle
      -- factors
    fftCore st u ts n vec

-- | Radix-2 Decimation-In-Frequency Inverse Fast Fourier Transformation of the
-- given complex vector. The given vector must be power-of-two sized, (for
-- example 2, 4, 8, 16, 32, etc.) The output is divided with the input size,
-- thus giving @`ifft` . `fft` == id@.
--
-- The length of the vector must be divisible by the number of unrolling steps.
--
-- The optimal amount of unrolling depends on the target architecture, but a
-- value of 2 might be a reasonable default that gives some performance
-- improvements on many systems and doesn't lead to too much code size increase.
ifft
    :: ( Manifestable Run vec (Data (Complex a))
       , Finite vec
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       )
    => Store (Data (Complex a))
    -> Length  -- ^ Unrolling steps in inner loops (1 means no unrolling)
    -> vec
    -> Run (DPull (Complex a))
ifft st u vec = do
    n  <- shareM (ilog2 (length vec))
    ts <- manifestFresh $ Pull (twoTo (n-1)) (tw True (twoTo n))
      -- Change `manifestFresh` to `return` to avoid pre-computing twiddle
      -- factors
    normalize <$> fftCore st u ts n vec
  where
    normalize = map (/ (i2n $ length vec))

