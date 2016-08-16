{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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
  ( Storage
  , fft
  , ifft
  ) where



import Prelude ()

import Feldspar.Run
import Feldspar.Data.Vector



-- | Storage for vector-to-vector operations
--
-- * The first array is the storage for the result. It should be free before
--   the operation.
--
-- * The second array is temporary storage. It can hold the argument vector, and
--   it will be free after the operation.
type Storage a = (Arr a, Arr a)

swapStorage :: Storage a -> Storage a
swapStorage (a,b) = (b,a)

rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

riffle :: (Pully pull a, Syntax a) => Data Index -> pull -> Pull a
riffle = backPermute . const . rotBit

stages
    :: ( Manifestable m init
       , Finite (init a)
       , Pushy m vec a
       , Syntax a
       , MonadComp m
       )
    => Storage (Internal a)
    -> Pull i                    -- ^ Vector to loop over
    -> (i -> Manifest a -> vec)  -- ^ One stage
    -> init a                    -- ^ Initial state
    -> m (Manifest a)            -- ^ Final state
stages (resLoc,tmpLoc) as body init = do
    manifestStore resLoc init
    for (0,1,Excl n) $ \i -> do
        prev <- Manifest . slice 0 l <$> unsafeFreezeArr resLoc
        manifestStore tmpLoc $ toPush $ body (as!i) prev
        copyArr resLoc (slice 0 l tmpLoc)
    Manifest . slice 0 l <$> unsafeFreezeArr resLoc
  where
    n = length as
    l = length init

bitRev :: (Manifestable m vec, Finite (vec a), Syntax a, MonadComp m)
    => Storage (Internal a)
    -> Data Index
    -> vec a
    -> m (Manifest a)
bitRev store n = stages store (1...n) riffle

testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0

fftCore
    :: ( Manifestable m vec
       , Finite (vec (Data (Complex a)))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       , MonadComp m
       )
    => Storage (Complex a)
    -> Bool  -- ^ Inverse?
    -> Data Index
    -> vec (Data (Complex a))
    -> m (DManifest (Complex a))
fftCore store inv n = stages store (reverse (0...n)) step
  where
    step k vec = Pull (length vec) ixf
      where
        ixf i = testBit i k ? (twid * (b - a)) $ (a+b)
          where
            k'   = i2n k
            a    = vec ! i
            b    = vec ! (i `xor` k2)
            twid = polar 1 ((if inv then π else -π) * i2n (lsbs k' i) / i2n k2)
            k2   = 1 .<<. k'

fft'
    :: ( Manifestable m vec
       , Finite (vec (Data (Complex a)))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       , MonadComp m
       )
    => Storage (Complex a)
    -> Bool  -- ^ Inverse?
    -> vec (Data (Complex a))
    -> m (DManifest (Complex a))
fft' store inv v = do
    n <- shareM (ilog2 (length v) - 1)
    fftCore store inv n v >>= bitRev (swapStorage store) n

-- | Radix-2 Decimation-In-Frequency Fast Fourier Transformation of the given
-- complex vector. The given vector must be power-of-two sized, (for example 2,
-- 4, 8, 16, 32, etc.) The output is non-normalized.
fft
    :: ( Manifestable m vec
       , Finite (vec (Data (Complex a)))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       , MonadComp m
       )
    => Storage (Complex a) -> vec (Data (Complex a)) -> m (DManifest (Complex a))
fft store = fft' store False

-- | Radix-2 Decimation-In-Frequency Inverse Fast Fourier Transformation of the
-- given complex vector. The given vector must be power-of-two sized, (for
-- example 2, 4, 8, 16, 32, etc.) The output is divided with the input size,
-- thus giving 'ifft . fft == id'.
ifft
    :: ( Manifestable m vec
       , Finite (vec (Data (Complex a)))
       , RealFloat a
       , PrimType a
       , PrimType (Complex a)
       , MonadComp m
       )
    => Storage (Complex a) -> vec (Data (Complex a)) -> m (DPull (Complex a))
ifft store v = normalize <$> fft' store True v
  where
    normalize = map (/ (i2n $ length v))

