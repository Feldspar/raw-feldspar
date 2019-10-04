[![Build Status](https://travis-ci.org/Feldspar/raw-feldspar.png?branch=master)](https://travis-ci.org/Feldspar/raw-feldspar)

# Resource-AWare Feldspar

This package is a complete reimplementation and partly a redesign of the Feldspar EDSL (formerly implemented in [feldspar-language](http://hackage.haskell.org/package/feldspar-language)). Feldspar aims to raise the abstraction level of numeric processing and DSP. Its compiler generates efficient C code suitable for running on embedded targets.

## Installation

RAW-Feldspar can be installed directly from [Hackage](http://hackage.haskell.org/package/raw-feldspar):

    cabal new-install raw-feldspar

The installation can be sped up a bit (and the size of the installation reduced) by adding a flag to `language-c-quote` (a dependency of RAW-Feldspar):

    cabal new-install --constraint="language-c-quote -full-haskell-antiquotes" raw-feldspar

However, this flag is less relevant now with Cabal's Nix-style local builds.

## Getting started

The best way to learn how to use RAW-Feldspar at the moment is to look through the [examples](examples/). We suggest going through the files named "TutN_..." in ascending order. The files are well-documented.

The easiest way for users of `cabal` to get access to the examples is to run

    cabal unpack raw-feldspar

There is also some guidance in the [Haddock documentation](http://hackage.haskell.org/package/raw-feldspar).

The vector library is central to programming in Feldspar. Its general operation is explained in the [Haddock documentation](http://hackage.haskell.org/package/raw-feldspar/docs/Feldspar-Data-Vector.html), and many [examples](examples/) are using vectors.

### Hello world!

Here is the obligatory "Hello world!" to get you going:

```haskell
import qualified Prelude
import Feldspar.Run

helloWorld :: Run ()
helloWorld = printf "Hello world!\n"
```

The program can be run from GHCi:

    *Main> runCompiled helloWorld
    cc -std=c99 /tmp/edsl_16816927771714636915.c -o /tmp/edsl_16816927771714636915

    #### Running:
    Hello world!

Note the call to `cc` before the code is run. This requires you to have a C compiler installed.

Many programs can also be run without a C compiler, using `runIO`:

    *Main> runIO helloWorld
    Hello world!

If you just want to look at the beauty of the generated C code, you can instead run:

    *Main> icompile helloWorld
    #include <stdio.h>
    int main()
    {
        fprintf(stdout, "Hello world!\n");
        return 0;
    }

### Numeric computations

OK, since Feldspar is mostly about *computation*, we need one more example: a function computing the sum of the squares of the numbers from 1 to `n` (commonly known as the "Hello world" of vector fusion):

```haskell
import qualified Prelude
import Feldspar.Run
import Feldspar.Data.Vector

sumSq :: Data Word32 -> Data Word32
sumSq n = sum $ map (\x -> x*x) (1...n)
```

The meaning of the function can be understood by comparing it to the standard Haskell function

```haskell
sumSq :: Word32 -> Word32
sumSq n = sum $ map (\x -> x*x) [1..n]
```

(Note that `sum` and `map` have been redefined in Feldspar. However, they behave analogously to their counterparts for lists.)

In order to turn a pure function such as `sumSq` into a runnable program, we can use the construction `connectStdIO $ return . sumSq`. This results in a program that gets its input from `stdin` and prints its output to `stdout`:

    *Demo> icompile $ connectStdIO $ return . sumSq
    #include <stdint.h>
    #include <stdio.h>
    int main()
    {
        uint32_t v0;
        uint32_t state1;
        uint32_t v2;

        fscanf(stdin, "%u", &v0);
        state1 = 0;
        for (v2 = 0; v2 < (uint32_t) (1 < v0 + 1) * v0; v2++) {
            uint32_t let3;

            let3 = v2 + 1;
            state1 = let3 * let3 + state1;
        }
        fprintf(stdout, "%u", state1);
        return 0;
    }

Note how the whole `sumSq` computation has been fused into a single loop without any array allocation.

## External libraries

### Zeldspar

[Zeldspar](https://github.com/koengit/zeldspar) is an implementation of the [Ziria DSL](http://dx.doi.org/10.1145/2694344.2694368) for wireless programming on top of RAW-Feldspar.

### raw-feldspar-mcs

[raw-feldspar-mcs](https://github.com/kmate/raw-feldspar-mcs) extends RAW-Feldspar and Zeldspar with multicore and scratchpad support.

The repository contains many [examples](https://github.com/kmate/raw-feldspar-mcs/tree/master/examples) written for the [Parallella](http://www.parallella.org) multicore architecture.

### feldspar-synch

[feldspar-synch](https://github.com/emilaxelsson/feldspar-synch) is a library that extends Feldspar with Yampa-style synchronous streams.

It contains a simple polyphonic synthesizer as a demonstration. The synthesizer may serve as a simple example of a complete (toy) application written in RAW-Feldspar. It also demonstrates how to make bindings to an external C library (the ALSA sound library).

## Why RAW-Feldspar?

The previous Feldspar implementation was split over three packages:

  * [feldspar-language](http://hackage.haskell.org/package/feldspar-language) -- the language front end
  * [feldspar-compiler](http://hackage.haskell.org/package/feldspar-compiler) -- the C-generating back end
  * [feldspar-io](https://github.com/emilaxelsson/feldspar-io) -- a monadic "IO" layer

(`feldspar-io`, which is still at an early stage of development, adds support for writing interactive programs calling external functions, etc.)

RAW-Feldspar is essentially a replacement of all three packages. It emerged as an exploration of two new ideas:

  * A new design in which memory usage is explicitly managed by the user
      - In the previous implementation, most array manipulation was done using pure functions, giving the user little chance to control memory usage and often leading to unwanted array copying.
  * Express the compiler as a translator to a typed low-level imperative EDSL
      - This makes the compiler both safer and more flexible
      - Read about the technique: [Compilation as a Typed EDSL-to-EDSL Transformation](http://fun-discoveries.blogspot.se/2016/03/compilation-as-typed-edsl-to-edsl.html)

RAW-Feldspar has since become a respectable replacement of the previous implementation. RAW-Feldspar typically generates slicker code based on native types and functions. Due to the new design, the user also has more control over array allocations, leading to lower memory usage and fewer array copies.

However, RAW-Feldspar also has some [limitations and lacks some features](https://github.com/Feldspar/raw-feldspar/wiki/Limitations-and-Missing-Features) compared to the previous version. Some features are missing simply because they have not been ported yet; others are missing for more fundamental reasons.

## Limitations and missing features

See [limitations and missing features](https://github.com/Feldspar/raw-feldspar/wiki/Limitations-and-Missing-Features).

There is also a [list of possible enhancements and fixes](https://github.com/Feldspar/raw-feldspar/wiki/TODOs).

## Implementation

The implementation of RAW-Feldspar builds heavily on three generic packages:

  * [syntactic](http://hackage.haskell.org/package/syntactic), providing:
      - a generic deep embedding of pure expressions
      - generic optimizations
      - etc.
  * [operational-alacarte](http://hackage.haskell.org/package/operational-alacarte), providing:
      - a generic deep embedding of monadic programs (based on the "Operational monad")
  * [imperative-edsl](http://hackage.haskell.org/package/imperative-edsl), providing:
      - operational instructions for imperative programs
      - C code generation
      - etc.

`imperative-edsl` is used both to represent monadic Feldspar programs and the low-level imperative code produced by the Feldspar compiler (following the idea in [Compilation as a Typed EDSL-to-EDSL Transformation](http://fun-discoveries.blogspot.se/2016/03/compilation-as-typed-edsl-to-edsl.html)).

The implementation also makes heavy use of the philosophy described in [Combining Deep and Shallow Embedding of Domain-Specific Languages](http://dx.doi.org/10.1016/j.cl.2015.07.003). The basic idea is to have a low-level core language -- the deep embedding -- and to build the user interface as shallow extensions on top of the core language.

A prime example of the technique is the [vector library](http://hackage.haskell.org/package/raw-feldspar/docs/Feldspar-Data-Vector.html), which provides high-level vector representations with a rich programming interface. These vectors only exist in the meta-language (i.e. Haskell), and by the time the Feldspar compiler is called, the vectors are already gone and what is left is imperative code with highly optimized loops. We saw an example of this when compiling the `sumSq` example above.

Another example is [feldspar-synch](https://github.com/emilaxelsson/feldspar-synch), which extends Feldspar with synchronous streams. The whole package is implemented as a shallow extension on top of RAW-Feldspar.

