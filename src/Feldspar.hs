module Feldspar
  ( module Prelude.EDSL
  , module Control.Monad
    -- * Types
  , module Data.Int
  , module Data.Word
  , Internal
  , Type
  , SmallType
  , Length
  , Index
  , Ref
  , Arr
  , Data
  , Syntax
  , Comp
  , Software
  , Hardware
    -- * Front end
  , module Feldspar.Frontend
  , module Language.Embedded.Imperative.Frontend.General
    -- * Storable types
  , module Feldspar.Storable
    -- * Back ends
  , runIO
  , compile
  , icompile
  , compileAndCheck'
  , compileAndCheck
  , runCompiled'
  , runCompiled
  , captureCompiled'
  , captureCompiled
  , compareCompiled'
  , compareCompiled
  ) where

import Prelude.EDSL

import Control.Monad

import Data.Int
import Data.Word

import Language.Syntactic

import Language.Embedded.Imperative.Frontend.General hiding (Ref, Arr)

import Feldspar.Representation
import Feldspar.Compile.Software
import Feldspar.Frontend
import Feldspar.Storable

