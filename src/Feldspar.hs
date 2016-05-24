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
  , PrimType'
  , PrimType
  , Type
  , Length
  , Index
  , Internal
  , Ref
  , Arr
  , IArr
  , Inhabited
    -- * Front end
  , eval
  , module Feldspar.Frontend
  , Integral
  , Fractional (..)
  , Floating (..)
  , Border (..)
  , IxRange
    -- * Storable types
  , module Feldspar.Storable
  ) where

import Prelude.EDSL hiding (negate)
import Prelude hiding (pi)

import Control.Monad

import Data.Int
import Data.Word

import Language.Syntactic

import Language.Embedded.Imperative (Border (..), IxRange)

import Data.Inhabited
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Storable

