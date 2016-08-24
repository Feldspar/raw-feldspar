module Tut1_HelloWorld where

-- This file demonstrates:
--
-- * How to write a very simple program
-- * How to run the program
-- * How to generate C code

import Prelude ()

import Feldspar.Run



-- Interactive programs run in the `Run` monad.

helloWorld :: Run ()
helloWorld = printf "Hello world!\n"

-- `runCompiled` generates C code behind the scene, calls the C compiler and
-- then runs the resulting executable.

test1 = runCompiled helloWorld

-- Many programs can also be run directly in Haskell, without generating C code:

test2 = runIO helloWorld

-- If you want to see the generated code:

test3 = putStrLn $ compile helloWorld

-- Or shorter:

test4 = icompile helloWorld

-- For testing:
testAll = do
    test1
    test2
    test3
    test4
    compareCompiled helloWorld (putStrLn "Hello world!") ""
      -- `compareCompiled` is used to compare the output written to `stdout`
      -- from a Feldspar program with that from a reference Haskell `IO`
      -- program. The last argument will be fed to `stdin` to both the Feldspar
      -- program and the reference.
    compareCompiled helloWorld (runIO helloWorld) ""
      -- The reference can also be obtained using `runIO`, which gives a way to
      -- test that the Feldspar compiler preserves the semantics of `runIO`.

