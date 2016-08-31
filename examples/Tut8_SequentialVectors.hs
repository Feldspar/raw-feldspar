{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tut8_SequentialVectors where

-- This file demonstrates how to use sequential vectors.

-- If you're looking for infinite streams, please see the library:
-- <https://github.com/emilaxelsson/feldspar-synch>

import qualified Prelude

import Feldspar.Run
import Feldspar.Data.Vector



-- The vector representations `Pull`, `Push` etc. are designed with
-- data-parallel programming in mind. However, it is quite common that elements
-- in a vector depend on each other in a sequential fashion, for example, when
-- filtering signals in the time domain. That is what sequential vectors are
-- for. You can think of sequential vectors as those that can be produced from
-- `scan`-like operations.



--------------------------------------------------------------------------------

-- The first `n` Fibonacci numbers using `unfold`:

fibSeq :: MonadComp m => Data Length -> DSeq m Index
fibSeq n = unfold n (\(a,b) -> ((b,a+b),a)) (0,1)

fibSeqRun = connectStdIO $ return . (fibSeq :: _ -> _ Run _)

comp_fibSeq = icompile fibSeqRun

run_fibSeq = runCompiled fibSeqRun



--------------------------------------------------------------------------------

-- Common filters are defined for sequential vectors in the module
-- `Feldspar.Processing.Filters`.



--------------------------------------------------------------------------------

testAll = do
    comp_fibSeq
    compareCompiled fibSeqRun (putStr "10 0 1 1 2 3 5 8 13 21 34 ") "10\n"
    compareCompiled fibSeqRun (runIO fibSeqRun) "10\n"

