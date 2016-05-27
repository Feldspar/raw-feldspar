{-# LANGUAGE TemplateHaskell #-}

-- | Inhabited types

module Data.Inhabited where



import Data.Int
import Data.Word

import Data.Inhabited.TH

import qualified Language.Embedded.Hardware.Expression.Represent.Bit as Hard

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Inhabited types
class Inhabited a
  where
    -- | Example value. An example value does not have to be an agreed-upon
    -- default value. It can really be any value whatsoever.
    example :: a

instance Inhabited Bool   where example = False
instance Inhabited Float  where example = 0
instance Inhabited Double where example = 0
instance Inhabited Int8   where example = 0
instance Inhabited Int16  where example = 0
instance Inhabited Int32  where example = 0
instance Inhabited Int64  where example = 0
instance Inhabited Word8  where example = 0
instance Inhabited Word16 where example = 0
instance Inhabited Word32 where example = 0
instance Inhabited Word64 where example = 0

instance (KnownNat n) => Inhabited (Hard.Bits n) where example = Hard.bitFromInteger 0

inhabitedTupleInstances 15

--------------------------------------------------------------------------------
