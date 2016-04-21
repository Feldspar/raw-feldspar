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
  , Complex (..)
  , PrimType'
  , PrimType
  , Type
  , Length
  , Index
  , Ref
  , Arr
  , IArr
  , Inhabited
  , Syntactic
  , Domain
  , Internal
    -- * Front end
  , eval
  , showAST
  , drawAST
  , module Feldspar.Frontend
  , Bits
  , FiniteBits
  , Integral
  , Ord
  , RealFloat
  , RealFrac
  , Border (..)
  , IxRange
    -- * Storable types
  , module Feldspar.Storable
  ) where

import Prelude.EDSL

import Control.Monad

import Data.Bits (Bits, FiniteBits)
import Data.Complex (Complex (..))
import Data.Int
import Data.Word

import Language.Syntactic (Syntactic, Domain, Internal)
import qualified Language.Syntactic as Syntactic

import Language.Embedded.Imperative (Border (..), IxRange)

import Data.Inhabited
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Optimize
import Feldspar.Storable



-- | Show the syntax tree using Unicode art
showAST :: (Syntactic a, Domain a ~ FeldDomain) => a -> String
showAST = Syntactic.showAST . optimize . Syntactic.desugar

-- | Draw the syntax tree on the terminal using Unicode art
drawAST :: (Syntactic a, Domain a ~ FeldDomain) => a -> IO ()
drawAST = Syntactic.drawAST . optimize . Syntactic.desugar

