{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- Copyright (c) 2016 and after, see package copyright

-- Copyright (c) 2013, Emil Axelsson, Peter Jonsson, Anders Persson and
--                     Josef Svenningsson
-- Copyright (c) 2012, Emil Axelsson, Gergely Dévai, Anders Persson and
--                     Josef Svenningsson
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    => Bool  -- ^ Inverse?
    -> Data Index
    -> Vector (Data (Complex Double))
    -> m (Vector (Data (Complex Double)))
fftCore inv n = stages (reverse (0...n)) step
  where
    step k vec = Indexed (length vec) ixf
      where
        ixf i = testBit i k ? (twid * (b - a)) $ (a+b)
          where
            k'   = i2n k
            a    = vec ! i
            b    = vec ! (i `xor` k2)
            twid = polar 1 ((if inv then π else -π) * i2n (lsbs k' i) / i2n k2)
            k2   = 1 .<<. k'

fft' :: MonadComp m
     => Bool  -- ^ Inverse?
     -> Vector (Data (Complex Double))
     -> m (Vector (Data (Complex Double)))
fft' inv v = do
    n' <- force n
    fftCore inv n' v >>= bitRev n'
  where
    n = ilog2 (length v) - 1


-- | Radix-2 Decimation-In-Frequeny Fast Fourier Transformation of the given
-- complex vector. The given vector must be power-of-two sized, (for example 2,
-- 4, 8, 16, 32, etc.) The output is non-normalized.
fft :: MonadComp m =>
    Vector (Data (Complex Double)) -> m (Vector (Data (Complex Double)))
fft = fft' False


-- | Radix-2 Decimation-In-Frequeny Inverse Fast Fourier Transformation of the
-- given complex vector. The given vector must be power-of-two sized, (for
-- example 2, 4, 8, 16, 32, etc.) The output is divided with the input size,
-- thus giving 'ifft . fft == id'.
ifft :: MonadComp m =>
    Vector (Data (Complex Double)) -> m (Vector (Data (Complex Double)))
ifft v = normalize <$> fft' True v
  where
    normalize = map (/ (i2n $ length v))


---

prog :: Run ()
prog = do
    n  <- fget stdin
--     let n = 31
    v  <- force $ map i2n (0...n)
    v' <- fft v
    printf "%.10f\n" $ sum $ map abs $ map realPart v'


---

transformFile :: String -> Bool -> Run ()
transformFile inputFile forward = do
    h <- fopen inputFile ReadMode
    n :: Data Length <- fget h
    printf "%d\n" n
    input :: Arr (Complex Double) <- newArr n
    for (0, 1, Excl n) $ \i -> do
        re :: Data Double <- fget h
        im :: Data Double <- fget h
        setArr i (complex re im) input

    v <- unsafeFreezeVec n input
    output <- (if forward then fft else ifft) v

    for (0, 1, Excl n) $ \i -> do
        let xi :: Data (Complex Double) = output ! i
            re = realPart xi
            im = imagPart xi
        printf "%f %f\n" re im
    fclose h

transformFileCompiled inputFile forward
    = runCompiled' opts $ transformFile inputFile forward
  where
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lm"]}

{- expected results, for all a/b/c test file variants:

transformFileCompiled "examples/FFT_in8a.txt" True == "examples/FFT_out8a.txt"

and

transformFileCompiled "examples/FFT_out8a.txt" False == "examples/FFT_in8a.txt"

where '==' means that the files are the same, apart from numerical errors.

-}

