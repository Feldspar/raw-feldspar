{-# LANGUAGE QuasiQuotes #-}

module Demo where



import qualified Prelude
import Control.Applicative ((<$>))

import Feldspar.Software
import Feldspar.Vector
import Feldspar.Option



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
    s <- getRef sum
    printf "The sum of your numbers is %d.\n" (s :: Data Word32)

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

type Array a = (Data Length, IArr a)

indexO :: Syntax a => Array (Internal a) -> Data Index -> Option a
indexO (len,arr) i = Option (i<len) (arrIx arr i)

funO :: Array Int32 -> Data Index -> Option (Data Int32)
funO arr i = do
    a <- indexO arr i
    b <- indexO arr (i+1)
    c <- indexO arr (i+2)
    d <- indexO arr (i+3)
    return (a+b+c+d)

test_option :: Software ()
test_option = do
    a <- unsafeFreezeArr =<< initArr [1..10]
    let arr = (10,a) :: Array Int32
    i <- store 4
    b <- fromSomeAssert "out of bounds" $ funO arr i
    printf "%d\n" b

------------------------------------------------------------

testAll = do
    compareCompiled sumInput     (runIO sumInput) (Prelude.unlines $ Prelude.map show $ Prelude.reverse [0..20])
    compareCompiled printFib     (runIO printFib)     "7\n"
    compareCompiled test_scProd1 (runIO test_scProd1) "20\n"
    compareCompiled test_scProd2 (runIO test_scProd2) "20\n"
    compareCompiled map_inplace  (runIO map_inplace)  ""
    compareCompiled test_option  (runIO test_option)  ""

