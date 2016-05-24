-- | ...

module Feldspar.Primitive.Backend.VHDL where

import Language.Embedded.Hardware
import Language.Embedded.Hardware.Command.Backend.VHDL

import Feldspar.Primitive.Representation

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

instance CompileType PrimType'
  where
    compileType = undefined
    compileLit  = undefined

instance CompileExp Prim
  where
    compE = undefined

--------------------------------------------------------------------------------
