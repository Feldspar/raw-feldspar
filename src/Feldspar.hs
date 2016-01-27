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
    -- * Front end
  , module Feldspar.Frontend
  , Border (..)
  , IxRange
    -- * Storable types
  , module Feldspar.Storable
  ) where

import Prelude.EDSL

import Control.Monad

import Data.Int
import Data.Word

import Language.Syntactic

import Language.Embedded.Imperative (Border (..), IxRange)

import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Storable

