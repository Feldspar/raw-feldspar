{-# LANGUAGE QuasiQuotes #-}

module Tut7_ImperativeProgramming where

-- This file demonstrates
--
-- * Imperative programming using loops, references and mutable arrays
-- * FFI: Binding to external C libraries
-- * Including C definitions

-- TODO Add example with mutable arrays.

import qualified Prelude

import Feldspar.Run

import Tut2_ExpressionsAndTypes (fib)



--------------------------------------------------------------------------------

printFib :: Run ()
printFib = do
    printf "Enter a positive number: "
    n <- readStd
    printf "The %dth Fibonacci number is %d.\n" n (fib n)

-- Note that `printf` accepts a variable number of arguments (like the `printf`
-- in normal Haskell).



--------------------------------------------------------------------------------

-- Demonstration of mutable references and while-loops.

sumInput :: Run ()
sumInput = do
    done <- initRef false
    sum  <- initRef (0 :: Data Word32)
    while (not <$> getRef done) $ do
        printf "Enter a number (0 means done): "
        n <- readStd
        iff (n == 0)
          (setRef done true)
          (modifyRef sum (+n))
    s <- getRef sum
    printf "The sum of your numbers is %d.\n" (s :: Data Word32)



--------------------------------------------------------------------------------

-- Binding to external C libraries:

abort :: Run ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

-- It possible to add C functions to the generated code and use `callProc` to
-- call them:

printRef :: Ref Word32 -> Run ()
printRef s = do
    addInclude "<stdio.h>"
    addDefinition printRef_def
    callProc "printRef" [refArg s]
  where
    printRef_def = [cedecl|
        void printRef (typename uint32_t * s) {
            printf ("The value of the reference is %d.\n", *s);
        }
        |]

test_printRef = do
    r <- initRef (22 :: Data Word32)
    printRef r

-- Note that code using `addDefinition`, `callProc` etc. cannot be run using
-- `runIO`.



--------------------------------------------------------------------------------

testAll = do
    compareCompiled printFib (runIO printFib) "7\n"
    compareCompiled sumInput (runIO sumInput) (Prelude.unlines $ Prelude.map show $ Prelude.reverse [0..20])
    icompile abort
    icompile test_printRef
    compareCompiled test_printRef (putStrLn "The value of the reference is 22.") ""

