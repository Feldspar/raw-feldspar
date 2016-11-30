{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tut5_Matrices where

-- This file demonstrates programming with matrices.

-- Please get acquainted with the examples covering vectors and memory
-- management before trying out these examples.

import Prelude ()

import Feldspar.Run
import Feldspar.Data.Vector
import Feldspar.Data.Buffered

import Tut2_ExpressionsAndTypes (mathOpts)
import DFT
import FFT



--------------------------------------------------------------------------------

-- The following function defines matrix multiplication (available as `matMul`
-- in the vector library):

mmul :: (Pully2 vec1 a, Pully2 vec2 a, Num a, Syntax a) =>
    vec1 -> vec2 -> Pull2 a
mmul veca vecb = Pull2 (numRows va) (numCols vb) $ \i j ->
    scProd (va!i) (transpose vb ! j)
  where
    va = toPull2 veca
    vb = toPull2 vecb

-- The constructor `Pull2` constructs a matrix given
--
-- * The number of rows
-- * The number of columns
-- * A function mapping row index and column index to element

mmulRun = connectStdIO $
    return . uncurry (mmul :: DPull2 Int32 -> DPull2 _ -> _)



--------------------------------------------------------------------------------

-- The file examples/DFT.hs provides an interesting example of matrix programming. The
-- DFT function is expressed as a multiplication of a transformation matrix with
-- the input vector. The matrix is never manifested, and the resulting code is
-- a doubly-nested loop where the matrix elements appear in the innermost
-- calculation.

dftRun = connectStdIO $ return . (dft :: DPull (Complex Double) -> _)



--------------------------------------------------------------------------------

-- We are often interested in doing something for each row or column (or some
-- other partitioning) of a matrix. This can be done using a combination of
-- `exposeRows` and `hideRows` (and `transpose` in case one is interested in the
-- columns).

-- The following function reverses each row in a matrix:

revRows :: (Pully2 vec a, Finite2 vec, MonadComp m) => vec -> Push2 m a
revRows vec
    = hideRows (numCols vec)
    $ map reverse
    $ exposeRows
    $ vec

revRowsRun = connectStdIO $ return . (revRows :: DPull2 Int32 -> Push2 Run _)

run_revRows = runCompiled revRowsRun

-- When running the program, the matrix is entered by first giving its number of
-- rows (r), then the number of columns (c), then the product r*c, and finally
-- the elements in order row by row. For example, this is a valid matrix:
-- "2 2 4 1 2 3 4\n".



--------------------------------------------------------------------------------

-- What if we want to do an effectful operation, such as FFT (see
-- examples/FFT.hs), on the rows of a matrix? This is possible using the
-- function `sequens`, which lets a push vector "eat" up the effects on its
-- elements (read the Haddock documentation for `Push` and `sequens` to see the
-- potential pitfalls of effectful push vectors).

-- The following function applies FFT to each column of a matrix:

fftCols :: DManifest2 (Complex Double) -> Run (DManifest2 (Complex Double))
fftCols vec = do
    st <- newStore (numRows vec)
    manifestFresh2
      $ transposePush
      $ hideRows (numRows vec)
      $ sequens
      $ map (fft st 1)
      $ exposeRows
      $ transpose
      $ vec

fftColsRun = connectStdIO fftCols



--------------------------------------------------------------------------------

testAll = do
    icompile mmulRun
    compareCompiled mmulRun (putStr "2 2 4 19 22 43 50 ") "2 2 4 1 2 3 4 2 2 4 5 6 7 8\n"
    compareCompiled mmulRun (runIO mmulRun)               "2 2 4 1 2 3 4 2 2 4 5 6 7 8\n"
    icompile dftRun
    captureCompiled' def mathOpts dftRun "4 11 12 13 14\n"
    icompile revRowsRun
    compareCompiled revRowsRun (putStr "2 2 4 6 5 8 7 ") "2 2 4 5 6 7 8\n"
    compareCompiled revRowsRun (runIO revRowsRun)        "2 2 4 5 6 7 8\n"
    icompile fftColsRun
    captureCompiled' def mathOpts fftColsRun "2 3 6 11 12 13 14 15 16\n"
    return ()

