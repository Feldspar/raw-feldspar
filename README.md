[![Build Status](https://travis-ci.org/emilaxelsson/raw-feldspar.png?branch=master)](https://travis-ci.org/emilaxelsson/raw-feldspar)

# Resource-Aware Feldspar

This package is a prototype for a new design for Feldspar. It tries two things:

  * A new design in which all arrays have to be explicitly managed in the IO layer (the `Run` monad, which roughly corresponds to Haskell's `IO`)
  * Compilation as a typed translation to [imperative-edsl](https://github.com/emilaxelsson/imperative-edsl).

In this new design the IO layer is an integrated part of the Feldspar language (rather than just an extension, as in the [feldspar-io](https://github.com/emilaxelsson/feldspar-io) package), so this package can be seen as a combination of the existing packages [feldspar-language](http://hackage.haskell.org/package/feldspar-language), [feldspar-compiler](http://hackage.haskell.org/package/feldspar-compiler) and [feldspar-io](https://github.com/emilaxelsson/feldspar-io).

## Examples

See the [examples](examples/) directory.

## Installation

Here is a suggested incantation:

    git clone git@github.com:emilaxelsson/imperative-edsl
    git clone git@github.com:emilaxelsson/raw-feldspar
    cd raw-feldspar
    cabal sandbox init
    cabal sandbox add-source ../imperative-edsl
    cabal install --constraint="language-c-quote -full-haskell-antiquotes"

