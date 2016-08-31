{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tut2_ExpressionsAndTypes where

-- This file demonstrates:
--
-- * How to write pure functions in Feldspar
-- * Basic types and the `Syntax` class
-- * How to tell the compiler to link in necessary C libraries

import Prelude ()

import Feldspar.Run



--------------------------------------------------------------------------------

-- A function that doubles its argument:

double :: (PrimType a, Num a) => Data a -> Data a
double a = a*2

-- The type constructor `Data` is for pure Feldspar expressions. `Data a` is an
-- expression that will yield a value of type `a` when evaluated.

-- In order to compile `double`, we first need to monomorphize it. This can be
-- conveniently done using a partial type signature. Then we also need to get
-- the input from somewhere and do something with the output. A convenient way
-- to do this is to use `connectStdIO`, which gets the input from `stdin` and
-- writes the output to `stdout`.

doubleRun :: Run ()
doubleRun = connectStdIO $ return . (double :: _ Int32 -> _)

comp_double = icompile doubleRun

run_double  = runCompiled doubleRun



--------------------------------------------------------------------------------

type Point a = (a,a)

-- This function computes the distance between two points in the plane:

dist :: (PrimType a, Floating a) => Point (Data a) -> Point (Data a) -> Data a
dist (x1,y1) (x2,y2) = sqrt (dx**2 + dy**2)
  where
    dx = x1-x2
    dy = y1-y2

-- We need to uncurry `dist` before we can use `connectStdIO`:

distRun = connectStdIO $ return . uncurry (dist :: (_ Double, _) -> _)

comp_dist = icompile distRun

-- Note that there are no tuples in the generated code. The coordinates simply
-- become four separate variables. The variables are assigned by reading four
-- numbers in sequence from `stdin`.

-- In order to run compiled code that uses floating-point operations such as
-- `sqrt`, we must tell the C compiler to link in the C math library. We do that
-- as follows:

mathOpts = def {externalFlagsPost = ["-lm"]}

run_dist = runCompiled' def mathOpts distRun

-- The first argument to `runCompiled'` is the options to the Feldspar compiler
-- and the second argument is options to the external compiler. The overloaded
-- value `def` gives the default options for the given type.



--------------------------------------------------------------------------------

-- A function computing the nth Fibonacci number can be written using `forLoop`:

fib :: Data Word32 -> Data Word32
fib n = fst $ forLoop n (0,1) $ \_ (a,b) -> (b,a+b)

-- The arguments to `forLoop` are as follows, in order:
--
-- * The number of iterations
-- * The initial state
-- * A step function computing the next state from the loop index and the
--   current state

-- Note the use of ordinary Haskell tuples for constructing and matching on the
-- state. This is made possible by the `Syntax` class by which `forLoop` (and
-- many other functions) is overloaded.

fibRun = connectStdIO $ return . fib

comp_fib = icompile fibRun

-- Note that the pair in the state doesn't appear in the generated code; the
-- state simply consists of two separate variables.



--------------------------------------------------------------------------------

-- You'll see that many polymorphic Feldspar functions are overloaded by the
-- `Syntax` class; e.g.:
--
--     forLoop :: Syntax st => Data Length -> st -> (Data Index -> st -> st) -> st
--
-- Intuitively, `Syntax a` can be understood as any type that can be translated
-- to/from Feldspar expressions. Conversion is done using the following
-- functions:
--
--     desugar :: Syntax a => a -> Data (Internal a)
--     sugar   :: Syntax a => Data (Internal a) -> a
--
-- However, there is rarely a need to use these conversions explicitly since
-- functions such as `forLoop` do them internally.

-- For example, the two types
--
--     Data (Int32, Double)
--     (Data Int32, Data Double)
--
-- are isomorphic, and `desugar`/`sugar` convert between the two. However, the
-- latter type is almost always preferred, since it can be constructed and
-- destructed by the normal Haskell constructor `(,)`, as we saw in the `fib`
-- example.



--------------------------------------------------------------------------------

testAll = do
    comp_double
    compareCompiled doubleRun (putStr "12")     "6\n"
    compareCompiled doubleRun (runIO doubleRun) "8\n"
    comp_dist
    compareCompiled' def mathOpts distRun' (runIO distRun') "11 12 13 14\n"
    comp_fib
    compareCompiled fibRun (putStr "8")   "6\n"
    compareCompiled fibRun (runIO fibRun) "7\n"
  where
    -- A version of `runDist` that returns an integer to avoid rounding errors
    -- in `compareCompiled`
    distRun' = connectStdIO $ return . conv . uncurry dist
      where
        conv :: Data Double -> Data Word32
        conv = round . (*100000)

