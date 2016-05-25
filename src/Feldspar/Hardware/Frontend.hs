module Feldspar.Hardware.Frontend
  ( Hardware
  , MonadHardware(..)
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
newSig :: PrimType a => Hardware (Signal a)
newSig = newNamedSig "s"

-- | Create an uninitialized named signal.
newNamedSig :: PrimType a => String -> Hardware (Signal a)
newNamedSig name = Hardware $ Hard.newNamedSignal name

-- | Create an initialized signal.
initSig :: PrimType a => HData a -> Hardware (Signal a)
initSig = initNamedSig "s"

-- | Create an initialized named signal.
initNamedSig :: PrimType a => String -> HData a -> Hardware (Signal a)
initNamedSig name e = Hardware $ Hard.initNamedSignal name e

-- | Get the contents of a signal.
getSig :: PrimType a => Signal a -> Hardware (HData a)
getSig = Hardware . Hard.getSignal

-- | Set the contents of a signal.
setSig :: PrimType a => Signal a -> HData a -> Hardware ()
setSig s = Hardware . Hard.setSignal s

-- | Modify the contents of a signal.
modifySig :: PrimType a => Signal a -> (HData a -> HData a) -> Hardware ()
modifySig s f = setSig s . f =<< unsafeFreezeSig s

-- | Freeze the contents of a signal.
unsafeFreezeSig :: PrimType a => Signal a -> Hardware (HData a)
unsafeFreezeSig = Hardware . Hard.unsafeFreezeSignal

--------------------------------------------------------------------------------
