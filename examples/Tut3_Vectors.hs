{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tut3_Vectors where

-- This file demonstrates:
--
-- * Basic usage of the vector library
-- * Safety assertions in the generated code

import Prelude ()

import Feldspar.Run
import Feldspar.Data.Vector



-- The vector library will be used by most Feldspar programs that process data
-- in vector or matrix form. It provides a high-level interface, largely
-- by Haskell's list library.

-- The vector library consists of three different vector types, each one in a
-- 1-dimensional and a 2-dimensional form:
--
-- * `Manifest` / `Manifest2`
-- * `Pull` / `Pull2`
-- * `Push` / `Push2`
--
-- In the common case when the element type is `Data`, we can use the following
-- shorthands:
--
-- * `DManifest` / `DManifest2`
-- * `DPull` / `DPull2`
-- * `DPush` / `DPush2`
--
-- I.e. we can write `DPull Word32` instead of `Pull (Data Word32)`.
--
-- Each vector type has a different set of operations defined, and the
-- operations are chosen so that each type only supports those which can be
-- carried out rather efficiently for that type.
--
-- Conversion from `Manifest` to `Pull` and from `Pull` to `Push` is always
-- cheap. However, conversion from `Pull`/`Push` to `Manifest` involves writing
-- the whole vector to memory.
--
-- In most cases, the types will guide which type to use when, and conversions
-- are done automatically. For example, `++` is only supported by `Push`, but
-- the operator accepts any two vectors that can be converted to `Push` and it
-- will do the conversion automatically.
--
-- The `Pull` and `Push` types enjoy *guaranteed* fusion. That is, all
-- intermediate vectors of type `Pull` or `Push` are guaranteed not to appear in
-- the generated code. Only `Manifest` vectors will. However, there are cases
-- when fusion leads to duplicated computations, which means that the user may
-- sometimes want to convert to `Manifest` even though this is not demanded by
-- the types.

-- This file gives an introduction to vectors and some illustrating examples.
-- For more detailed information see the documentation:
-- <http://hackage.haskell.org/package/raw-feldspar/docs/Feldspar-Data-Vector.html>



--------------------------------------------------------------------------------

-- Pull vectors support many list-like operations. Here is a function that sums
-- the last 5 elements in a vector:

sumLast5 :: (Num a, Syntax a) => Pull a -> a
sumLast5 = sum . take 5 . reverse

comp_sumLast5 = icompile $ connectStdIO $ return . (sumLast5 :: _ -> Data Int32)

-- Note that all intermediate pull vectors in `sumLast5` have been fused away.
-- The only array in the generated code is the one that holds the input.

sumLast5Run = connectStdIO $ return . (sumLast5 :: _ -> Data Int32)

run_sumLast5 = runCompiled sumLast5Run

-- When running the program, the vector is entered by first giving its length
-- and then as many elements as the given length. For example, this is a valid
-- vector: "4 1 2 3 4\n".

-- If we want the function to work also for `Manifest` vectors, we just change
-- the type:

sumLast5' :: (Pully vec a, Num a, Syntax a) => vec -> a
sumLast5' = sum . take 5 . reverse



--------------------------------------------------------------------------------

-- Compute the sum of the square of the numbers from 1 to n:

sumSq :: Data Word32 -> Data Word32
sumSq n = sum $ map (\x -> x*x) (1...n)

sumSqRun = connectStdIO $ return . sumSq

comp_sumSq = icompile sumSqRun

-- Note that there is not a single array declared in the generated code, only
-- scalars.

-- The type `Word32` is used a lot in the vector library. However, since it
-- usually represents either an index or a length, it goes under the aliases:
-- `Index` and `Length`.



--------------------------------------------------------------------------------

-- Dot product of two vectors:

dotProd :: (Num a, Syntax a) => Pull a -> Pull a -> a
dotProd a b = sum $ zipWith (*) a b

dotProdRun = connectStdIO $ return . uncurry (dotProd :: _ -> _ -> Data Int32)

comp_dotProd = icompile dotProdRun

-- Again, if we want the function to work also for `Manifest` vectors, we just
-- change the type:

dotProd' :: (Pully vec1 a, Pully vec2 a, Num a, Syntax a) => vec1 -> vec2 -> a
dotProd' a b = sum $ zipWith (*) a b

-- This function is available under the name `scProd` in the vector library.



--------------------------------------------------------------------------------

-- Pull vectors support arbitrary reading patterns. For example, `sumLast5`
-- demonstrated how to read a part from the end of a vector.

-- Push vectors, on the other hand, support arbitrary write patterns. For
-- example `++` creates a vector that writes its content using two separate
-- loops -- one for the left operand and one for the right operand.

-- The following function appends two modified compies of a vector to each
-- other:

quirk :: (Pully vec a, Num a, MonadComp m) => vec -> Push m a
quirk vec = reverse vec ++ map (*2) vec

-- `MonadComp` is any monad to which the monad `Comp` can be lifted. `Comp` can
-- be seen as a restricted version of `Run`, supporting only mutable data
-- structures (similarly to `ST` in normal Haskell). Both `Comp` and `Run` are
-- instances of `MonadComp`.

-- The reason why `Push` is parameterized on the monad `m` is that it is
-- possible to embed side-effects into push vectors using the function
-- `sequens`. For more information, see the Haddock documentation for `sequens`.

quirkRun = connectStdIO $ return . (quirk :: DPull Int32 -> Push Run _)

comp_quirk = icompile quirkRun



--------------------------------------------------------------------------------

-- Manifest vectors have a direct representation in memory; i.e. they correspond
-- to an array in the generated code. Manifest vectors can be created in
-- different ways; e.g.:
--
-- * By reading from a file using `readStd` or `fread`
-- * By freezing a mutable vector using `unsafeFreezeSlice`
-- * By writing another vector to memory using `manifest` or `manifestFresh`

-- Writing a vector to memory is often forced by the types. For example, if we
-- want to compose `sumLast5` with `quirk` we find that the types don't match
-- (even if `sumLast5` were overloaded using `Pully`, since there is no instance
-- `Pully Push`).

demandedManifest :: (Num a, Syntax a, MonadComp m) => Pull a -> m a
demandedManifest vec = do
    vec2 <- manifestFresh $ quirk vec
    return $ sumLast5' vec2

-- Here, `vec2` is a manifest vector. One problem with this example is the use
-- of `manifestFresh` which silently allocates a fresh array to hold the
-- manifest vector. This array will be allocated on the stack and not freed
-- until the end of the current block in the generated code.

-- An alternative to `manifestFresh` is `manifest` which takes the location to
-- which the vector should be written as an argument. This allows reusing the
-- same array for different manifest vectors. However, such reuse is dangerous
-- since each call to `manifest` with a given location will invalidate earlier
-- manifest vectors using that location.
--
-- So the user has a choice: Either play safe and use `manifestFresh` or have
-- control over memory usage using `manifest`. Future plans involve
-- incorporating static analyses to rule out unsafe uses of `manifest` allowing
-- users to have both safety and control at the same time.

demandedManifestRun = connectStdIO (demandedManifest :: _ -> Run (Data Int32))

comp_demandedManifest = icompile demandedManifestRun



--------------------------------------------------------------------------------

-- The vector library contains many partial functions. In contrast to normal
-- Haskell, partial functions may not necessarily crash but may silently go on
-- producing bogus results.

-- For example, in this program, we take the head of an empty vector and
-- multiply it by 2:

bottom :: Data Word32 -> Data Word32
bottom n = a*2
  where
    a = head $ take 0 (1...n)

bottomRun = connectStdIO $ return . bottom

comp_bottom = icompile bottomRun

-- Looking at the generated code, we see that it always returns the number 2
-- without complaining.

run_bottom = runCompiled bottomRun

-- However, if we run the code, we'll find that it actually does crash, even
-- giving an informative message: "indexing outside of Pull vector".

-- The reason is that `icompile` and `runCompiled` actually pass different
-- options to the Feldspar compiler. In order to see the assertions in the
-- generated code, we can use `icompile'`:

comp_bottom_withAsserts =
    icompile' def {compilerAssertions = allExcept []} bottomRun

-- And in order to run the program without assertions, we can use
-- `runCompiled'`:

run_bottom_withoutAsserts =
    runCompiled' def {compilerAssertions = select []} def bottomRun

-- Unfortunately, compiling with assertions will usually prevent simplification
-- of the generated code. The compiler also does a poor job of sharing
-- assertions which is why the same assertion may appear many times in the
-- generated code.
--
-- For this reason, assertions are mostly intended to be used during testing.
-- When generating code for deployment, assertions will typically be turned off,
-- given that the code has been thoroughly tested.



--------------------------------------------------------------------------------

testAll = do
    comp_sumLast5
    compareCompiled sumLast5Run (putStr "80")       "7 12 13 14 15 16 17 18\n"
    compareCompiled sumLast5Run (runIO sumLast5Run) "7 22 23 24 25 26 27 28\n"
    comp_sumSq
    compareCompiled sumSqRun (putStr "385")   "10\n"
    compareCompiled sumSqRun (runIO sumSqRun) "20\n"
    comp_dotProd
    compareCompiled dotProdRun (putStr "794")     "3 11 12 13 4 21 22 23 24\n"
    compareCompiled dotProdRun (runIO dotProdRun) "3 11 12 13 4 31 32 33 34\n"
    comp_quirk
    compareCompiled quirkRun (putStr "6 4 3 2 4 6 8 ") "3 2 3 4\n"
    compareCompiled quirkRun (runIO quirkRun)          "3 12 13 14\n"
    comp_demandedManifest
    compareCompiled demandedManifestRun (putStr "23")               "3 2 3 4\n"
    compareCompiled demandedManifestRun (runIO demandedManifestRun) "3 12 13 14\n"
    icompile bottomRun
    comp_bottom_withAsserts

