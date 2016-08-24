module Tut4_MemoryManagement where

-- This file demonstrates how to control memory usage when writing vector code.

-- For a more realistic example of managed storage, see the example file
-- examples/FFT.hs.

-- For more detailed information on double-buffered storage, see the
-- documentation:
-- <http://hackage.haskell.org/package/raw-feldspar/docs/Feldspar-Data-Buffered.html>

-- Some of the examples show unsafe uses of functions, where the user is
-- expected to ensure proper usage. Future plans involve incorporating static
-- analyses to detect unsafe uses of memory.

import Prelude ()

import Feldspar.Run
import Feldspar.Data.Vector
import Feldspar.Data.Buffered

import Tut3_Vectors (quirk)



--------------------------------------------------------------------------------

-- A previous example showed the use of `manifestFresh` to store the content of
-- a vector in memory and get a manifest vector back as result. The problem with
-- this function is that it allocates an array under the hood. Often times,
-- vector operations are done in a "pipeline" consisting of many stages. In such
-- cases, one typically wants to allocate a small number of arrays and use
-- throughout the pipeline.

-- As an example, consider using the previously defined function `quirk` to
-- build up a vector in several steps, starting from the singleton vector `[n]`:

buildQuirk_safe :: Data Word32 -> Run (DManifest Word32)
buildQuirk_safe n = do
    vec <- listManifest [n]
    vec <- manifestFresh $ quirk vec
    vec <- manifestFresh $ quirk vec
    vec <- manifestFresh $ quirk vec
    vec <- manifestFresh $ quirk vec
    vec <- manifestFresh $ quirk vec
    return vec

-- (Note the use of shadowing to avoid coming up with unique variable names.)

comp_buildQuirk_safe = icompile $ connectStdIO buildQuirk_safe

-- Although this function is completely safe, allocating a fresh array for each
-- intermediate vector puts high pressure on the stack.



--------------------------------------------------------------------------------

-- Another alternative is to allocate *one* storage, and use the function
-- `store` to store the intermediate vectors:

buildQuirk_managed :: Data Word32 -> Run (DManifest Word32)
buildQuirk_managed n = do
    st  <- newStore 32
    vec <- store st $ listPush [n]
    vec <- store st $ quirk vec
    vec <- store st $ quirk vec
    vec <- store st $ quirk vec
    vec <- store st $ quirk vec
    vec <- store st $ quirk vec
    return vec

-- Note that it is now up to the user to make sure that the store is large
-- enough to fit each of the intermediate vectors.

-- Note also that we're using the same store throughout the code. Indeed, it
-- looks as though we're reading from and writing to the store at the same time.
-- The reason why this works is that a store actually consists of *two* arrays,
-- and the two buffers are automatically flipped (by pointer swapping) after
-- each `store`.

comp_buildQuirk_managed = icompile $ connectStdIO buildQuirk_managed



--------------------------------------------------------------------------------

-- For completeness, we also give an even more explicit version of the previous
-- functions:

buildQuirk_managed2 :: Data Word32 -> Run (DManifest Word32)
buildQuirk_managed2 n = do
    arr1 <- newArr 32
    arr2 <- newArr 32
    vec  <- manifest arr1 $ listPush [n]
    vec  <- manifest arr2 $ quirk vec
    vec  <- manifest arr1 $ quirk vec
    vec  <- manifest arr2 $ quirk vec
    vec  <- manifest arr1 $ quirk vec
    vec  <- manifest arr2 $ quirk vec
    return vec

comp_buildQuirk_managed2 = icompile $ connectStdIO buildQuirk_managed2

-- The difference to the previous version is that we now allocate the two
-- buffers explicitly, and manually make sure to use the buffers in alternating
-- order.

-- Essentially, the only time the above style is preferred is when we need to
-- take unsafe shortcuts. For example, if we replace `quirk` with a function
-- that can be performed in-place, we can get away with using a single buffer:

build_inplace :: Data Word32 -> Run (DManifest Word32)
build_inplace n = do
    arr <- newArr n
    vec <- manifest arr (1...n)
    vec <- manifest arr $ map (*2) vec
    vec <- manifest arr $ map (*3) vec
    vec <- manifest arr $ map (*4) vec
    vec <- manifest arr $ map (*5) vec
    vec <- manifest arr $ map (*6) vec
    return vec

comp_build_inplace = icompile $ connectStdIO build_inplace



--------------------------------------------------------------------------------

testAll = do
    comp_buildQuirk_safe
    compareCompiled (connectStdIO buildQuirk_safe) (putStr "32 80 40 20 40 20 10 20 40 20 10 5 10 20 10 20 40 80 40 20 40 20 10 20 40 80 40 20 40 80 40 80 160 ") "5\n"
    -- compareCompiled (connectStdIO buildQuirk_safe) (runIO $ connectStdIO buildQuirk_safe) "5\n"
      -- TODO Code generation too slow...
    comp_buildQuirk_managed
    compareCompiled (connectStdIO buildQuirk_managed) (putStr "32 80 40 20 40 20 10 20 40 20 10 5 10 20 10 20 40 80 40 20 40 20 10 20 40 80 40 20 40 80 40 80 160 ") "5\n"
    -- compareCompiled (connectStdIO buildQuirk_managed) (runIO $ connectStdIO buildQuirk_managed) "5\n"
      -- TODO Code generation too slow...
    comp_buildQuirk_managed2
    compareCompiled (connectStdIO buildQuirk_managed2) (putStr "32 80 40 20 40 20 10 20 40 20 10 5 10 20 10 20 40 80 40 20 40 20 10 20 40 80 40 20 40 80 40 80 160 ") "5\n"
    -- compareCompiled (connectStdIO buildQuirk_managed2) (runIO $ connectStdIO buildQuirk_managed2) "5\n"
      -- TODO Code generation too slow...
    comp_build_inplace
    compareCompiled (connectStdIO build_inplace) (putStr "5 720 1440 2160 2880 3600 ") "5\n"
    compareCompiled (connectStdIO build_inplace) (runIO $ connectStdIO build_inplace) "5\n"

