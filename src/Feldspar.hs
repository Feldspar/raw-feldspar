module Feldspar
  ( module Prelude.EDSL
  , module Control.Monad
    -- * Types
    -- ** Syntax
  , Data
  , Syntax
  , Comp
    -- ** Values
  , module Data.Int
  , module Data.Word
  , Internal
  , Type
  , SmallType
  , Length
  , Index
  , Ref
  , Arr
  , IArr
  , Inhabited
    -- * Front end
  , eval
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

import Data.Inhabited
import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Storable

