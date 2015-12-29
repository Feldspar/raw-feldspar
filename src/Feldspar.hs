module Feldspar
  ( module Prelude.EDSL
  , module Control.Monad
  , module Data.Int
  , module Data.Word
    -- * Front end
  , module Feldspar.Frontend
  , module Language.Embedded.Imperative.Frontend.General
    -- * Types
  , Internal
  , Type
  , SmallType
  , Length
  , Index
  , Ref
  , Arr
  , Data
  , Syntax
  , Program
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
import Feldspar.Compile
import Feldspar.Frontend

