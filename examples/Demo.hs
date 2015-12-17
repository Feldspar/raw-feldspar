{-# LANGUAGE QuasiQuotes #-}

module Demo where

import Prelude ()
import Control.Applicative ((<$>))

import Feldspar
import Feldspar.Vector



sumInput :: Program ()
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

abort :: Program ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

printSum :: Ref Word32 -> Program ()
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
run_sumInput  = compileAndRun [] sumInput []



------------------------------------------------------------

fib :: Data Word32 -> Data Word32
fib n = fst $ forLoop (i2n n) (0,1) $ \_ (a,b) -> (b,a+b)

printFib :: Program ()
printFib = do
    printf "Enter a positive number: "
    n <- fget stdin
    printf "The %dth Fibonacci number is %d.\n" n (fib n)



------------------------------------------------------------

test_scProd1 = printf "result: %f\n" $
    scProd (map i2n (0...19)) (map i2n (2...21))

test_scProd2 = do
    v1 <- store $ map i2n (0...19)
    v2 <- store $ map i2n (2...21)
    printf "result: %f\n" $ scProd v1 v2

