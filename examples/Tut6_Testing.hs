{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tut6_Testing where

-- This file demonstrates programming testing Feldspar functions with
-- QuickCheck.

import qualified Prelude

import Feldspar.Run
import Feldspar.Data.Vector

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC



-- The function `marshalled` can be used to turn a Feldspar function
-- `a -> Run b` into a corresponding Haskell function `a' -> IO b'`. The type
-- family `HaskellRep` is used to relate `a` and `a'` resp. `b` and `b'`:
-- `a' ~ HaskellRep a` and `b' ~ HaskellRep b`.

-- Since `marshalled` is defined in continuation passing style (in order to be
-- able to share generated files and clean up afterwards), it is convenient to
-- make our properties parameterized on the function under test.

-- The following property tests that a given function indeed computes the scalar
-- product:

prop_scProd :: (([Int32],[Int32]) -> IO Int32) -> Property
prop_scProd f = monadicIO $ do
    as <- pick arbitrary
    bs <- pick arbitrary
    x  <- run $ f (as,bs)
    QC.assert (x Prelude.== scProdList as bs)
  where
    scProdList as bs = Prelude.sum $ Prelude.zipWith (*) as bs

-- Note that we have to use QuickCheck's monadic API because the function under
-- test is in `IO`.



--------------------------------------------------------------------------------

scProdRun = return . uncurry (scProd :: DPull Int32 -> DPull _ -> _)

testAll =
    marshalled scProdRun $ \scProd' ->
      quickCheck $ prop_scProd scProd'

-- Due to the sharing provided by `marshalled`, the function `scProdRun` is only
-- compiled once and then `quickCheck` calls the resulting program 100 times.

