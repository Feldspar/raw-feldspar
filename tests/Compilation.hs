{-# LANGUAGE ScopedTypeVariables #-}

module Compilation where



import qualified Prelude

import Feldspar
import Feldspar.Software



-- Test that constant folding does not attempt to fold array indexing
test_constFoldArr :: Software ()
test_constFoldArr = do
    arr <- initIArr [1..10]
    let a :: Data Int32 = (arrIx arr 0 == arrIx arr 1) ? arrIx arr 100 $ arrIx arr 2
    printf "%d\n" a



testAll = do
    compareCompiled test_constFoldArr (runIO test_constFoldArr) ""

