module Feldspar.Hardware.Frontend
  ( Hardware
  , MonadHardware(..)

  , Signal
    
  , module Feldspar.Hardware.Frontend
  ) where

import Language.Embedded.Hardware (Signal)
import qualified Language.Embedded.Hardware as Hard

import qualified Control.Monad.Operational.Higher as Oper

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()
import Feldspar.Representation
import Feldspar.Hardware.Representation

--------------------------------------------------------------------------------
-- * Frontend for hardware specific operations.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | Create an uninitialized signal.
newSig :: HPrimType a => Hardware (Signal a)
newSig = newNamedSig "s"

-- | Create an uninitialized named signal.
newNamedSig :: HPrimType a => String -> Hardware (Signal a)
newNamedSig name = Hardware $ Hard.newNamedSignal name

-- | Create an initialized signal.
initSig :: HPrimType a => HData a -> Hardware (Signal a)
initSig = initNamedSig "s"

-- | Create an initialized named signal.
initNamedSig :: HPrimType a => String -> HData a -> Hardware (Signal a)
initNamedSig name e = Hardware $ Hard.initNamedSignal name e

-- | Get the contents of a signal.
getSig :: HPrimType a => Signal a -> Hardware (HData a)
getSig = Hardware . Hard.getSignal

-- | Set the contents of a signal.
setSig :: HPrimType a => Signal a -> HData a -> Hardware ()
setSig s = Hardware . Hard.setSignal s

-- | Modify the contents of a signal.
modifySig :: HPrimType a => Signal a -> (HData a -> HData a) -> Hardware ()
modifySig s f = setSig s . f =<< unsafeFreezeSig s

-- | Freeze the contents of a signal.
unsafeFreezeSig :: HPrimType a => Signal a -> Hardware (HData a)
unsafeFreezeSig = Hardware . Hard.unsafeFreezeSignal

--------------------------------------------------------------------------------
-- ** Components.

-- | Signature of a hardware component's interface, wrapping a hardware program.
type Signature = Hard.Sig  HardwareCMD HData HPrimType' (Oper.Program CompCMD (Oper.Param2 HData HPrimType'))

-- | Hardware componenets.
type Component = Hard.Comp HardwareCMD HData HPrimType' (Oper.Program CompCMD (Oper.Param2 HData HPrimType'))

-- | Typed list of arguments for a hardware component.
type Arguments = Hard.Arg

-- | Creates a named hardware component from the given signature.
namedComponent :: String -> Signature a -> Hardware (Component a)
namedComponent name = Hardware . Hard.namedComponent name

-- | Creates an component from the given signature.
component :: Signature a -> Hardware (Component a)
component = namedComponent "comp"

-- | Maps some inputs to an component.
portmap :: Component a -> Arguments a -> Hardware ()
portmap comp = Hardware . Hard.portmap comp

--------------------------------------------------------------------------------

uniqueInput  :: HPrimType' a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueInput  = Hard.exactInput

uniqueOutput :: HPrimType' a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueOutput = Hard.exactOutput

ret :: Hardware () -> Signature ()
ret = Hard.ret . unHardware

--------------------------------------------------------------------------------
