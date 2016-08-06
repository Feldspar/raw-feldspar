{-# LANGUAGE QuasiQuotes #-}

module Demo where



import qualified Prelude

import Feldspar.Run
import Feldspar.Data.Vector



sumInput :: Run ()
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

abort :: Run ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

printSum :: Ref Word32 -> Run ()
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
fib n = fst $ forLoop n (0,1) $ \_ (a,b) -> (b,a+b)

printFib :: Run ()
printFib = do
    printf "Enter a positive number: "
    n <- fget stdin
    printf "The %dth Fibonacci number is %d.\n" n (fib n)



------------------------------------------------------------

test_scProd1 = do
    n <- fget stdin
    printf "result: %.3f\n" $
      (scProd (fmap i2n (0 ... n-1)) (fmap i2n (2 ... n+1)) :: Data Double)

test_scProd2 = do
    n <- fget stdin
    v1 <- force $ fmap i2n (0 ... n-1)
    v2 <- force $ fmap i2n (2 ... n+1)
    printf "result: %.3f\n" (scProd v1 v2 :: Data Double)

map_inplace :: Run ()
map_inplace = do
    arr  <- newArr 20
    vec1 <- memorize arr Outer (0...19)
    arr' <- unsafeRecycleArr arr
    vec2 <- memorize arr' Outer $ map (*33) vec1
    printf "result: %d\n" $ sum $ toPull vec2
  -- This is not encouraged style, but it can be done



------------------------------------------------------------

testAll = do
    tag "sumInput"     >> compareCompiled sumInput     (runIO sumInput) (Prelude.unlines $ Prelude.map show $ Prelude.reverse [0..20])
    tag "printFib"     >> compareCompiled printFib     (runIO printFib)     "7\n"
    tag "test_scProd1" >> compareCompiled test_scProd1 (runIO test_scProd1) "20\n"
    tag "test_scProd2" >> compareCompiled test_scProd2 (runIO test_scProd2) "20\n"
    tag "map_inplace"  >> compareCompiled map_inplace  (runIO map_inplace)  ""
  where
    tag str = putStrLn $ "---------------- examples/Demo.hs/" Prelude.++ str Prelude.++ "\n"

