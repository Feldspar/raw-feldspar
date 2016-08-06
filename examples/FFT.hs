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

rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

riffle :: (Pully pull a, Syntax a) => Data Index -> pull -> Pull a
riffle = permute . const . rotBit

stages
    :: ( Finite (push1 a)
       , Materializable m (push1 a)
       , InnerElem (push1 a) ~ a
       , Dimensions (push1 a) ~ Dim ()
       , Materializable m (push2 a)
       , InnerElem (push2 a) ~ a
       , Dimensions (push2 a) ~ Dim ()
       , InnerElem a ~ a
       , Dimensions a ~ ()
       )
    => Storage (Internal a)
    -> Pull i                            -- ^ Vector to loop over
    -> (i -> Manifest a -> push2 a)      -- ^ One stage
    -> push1 a                           -- ^ Initial state
    -> m (Manifest a, Arr (Internal a))  -- ^ Final state
stages (resLoc,tmpLoc) as body init = do
    memorizeStore resLoc Outer init
    tmpLoc' <- recycleArr tmpLoc
    for (0,1,Excl n) $ \i -> do
      freezeArrTemp resLoc $ \resLocI -> do
        let prev = Manifest $ slice 0 l resLocI
        memorizeStore tmpLoc' Outer $ body (as!i) prev
      copyArr resLoc tmpLoc'
    res <- (Manifest . slice 0 l) <$> freezeArr resLoc
    return (res,tmpLoc')
  where
    n = length as
    l = length init

bitRev
    :: ( Pushy push
       , Syntax a
       , Finite (push a)
       , Materializable m a
       , InnerElem a ~ a
       , Dimensions a ~ ()
       , Materializable m (push a)
       , InnerElem (push a) ~ a
       , Dimensions (push a) ~ Dim ()
       , MonadComp m
       )
    => Storage (Internal a)
    -> Data Index
    -> push a
    -> m (Manifest a, Arr (Internal a))
bitRev store n = stages store (1...n) riffle

testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0

fftCore :: (RealFloat a, PrimType a, PrimType (Complex a), MonadComp m)
    => Storage (Complex a)
    -> Bool  -- ^ Inverse?
    -> Data Index
    -> DPull (Complex a)
    -> m (DManifest (Complex a), Arr (Complex a))
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

fft' :: (RealFloat a, PrimType a, PrimType (Complex a), MonadComp m)
    => Storage (Complex a)
    -> Bool  -- ^ Inverse?
    -> DPull (Complex a)
    -> m (DManifest (Complex a))
fft' (loc1,loc2) inv v = do
    n          <- force (ilog2 (length v) - 1)
    (v2,loc2') <- fftCore (loc1,loc2) inv n v
    fst <$> bitRev (loc2',loc1) n (toPull v2)

-- | Radix-2 Decimation-In-Frequency Fast Fourier Transformation of the given
-- complex vector. The given vector must be power-of-two sized, (for example 2,
-- 4, 8, 16, 32, etc.) The output is non-normalized.
fft :: (RealFloat a, PrimType a, PrimType (Complex a), MonadComp m) =>
    Storage (Complex a) -> DPull (Complex a) -> m (DManifest (Complex a))
fft store = fft' store False

-- | Radix-2 Decimation-In-Frequency Inverse Fast Fourier Transformation of the
-- given complex vector. The given vector must be power-of-two sized, (for
-- example 2, 4, 8, 16, 32, etc.) The output is divided with the input size,
-- thus giving 'ifft . fft == id'.
ifft :: (RealFloat a, PrimType a, PrimType (Complex a), MonadComp m) =>
    Storage (Complex a) -> DPull (Complex a) -> m (DPull (Complex a))
ifft store v = normalize <$> fft' store True v
  where
    normalize = fmap (/ (i2n $ length v)) . toPull

