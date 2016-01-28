{-# LANGUAGE QuasiQuotes #-}

module Demo where



import qualified Prelude
import Control.Applicative ((<$>))

import Feldspar
import Feldspar.Software
import Feldspar.Vector



sumInput :: Software ()
sumInput = do
    done <- initRef false
    sum  <- initRef (0 :: Data Word32)
    while (not <$> getRef done) $ do
        printf "Enter a number (0 means done): "
        n <- fget stdin
        iff (n == 0)
          (setRef done true)
          (modifyRef sum (+n))
--     abort
--     printSum sum
    printf "The sum of your numbers is %d.\n" =<< getRef sum

abort :: Software ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

printSum :: Ref Word32 -> Software ()
printSum s = do
    addDefinition printSum_def
    callProc "printSum" [refArg s]

printSum_def = [cedecl|
    void printSum (typename uint32_t * s) {
        printf ("I think the sum of your numbers is %d.\n", *s);
    }
    |]

-- Compiling and running:

comp_sumInput = icompile sumInput
run_sumInput  = runCompiled sumInput



------------------------------------------------------------

fib :: Data Word32 -> Data Word32
fib n = fst $ forLoop (i2n n) (0,1) $ \_ (a,b) -> (b,a+b)

printFib :: Software ()
printFib = do
    printf "Enter a positive number: "
    n <- fget stdin
    printf "The %dth Fibonacci number is %d.\n" n (fib n)



------------------------------------------------------------

test_scProd1 = do
    n <- fget stdin
    printf "result: %.3f\n" $
      scProd (map i2n (0 ... n-1)) (map i2n (2 ... n+1))

test_scProd2 = do
    n <- fget stdin
    v1 <- store $ map i2n (0 ... n-1)
    v2 <- store $ map i2n (2 ... n+1)
    printf "result: %.3f\n" $ scProd v1 v2

map_inplace :: Software ()
map_inplace = do
    svec <- initStore (0...19)
    inplace svec $ map (*33)
    vec <- readStore svec
    printf "result: %d\n" $ sum vec



------------------------------------------------------------

testAll = do
    compareCompiled sumInput (Prelude.unlines $ Prelude.map show $ Prelude.reverse [0..20])
    compareCompiled printFib "7\n"
    compareCompiled test_scProd1 "20\n"
    compareCompiled test_scProd2 "20\n"
    compareCompiled map_inplace ""

