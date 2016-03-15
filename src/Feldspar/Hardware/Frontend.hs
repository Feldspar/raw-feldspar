module Feldspar.Hardware.Frontend where

{-

module Feldspar.Hardware.Frontend
  ( Hardware
  , MonadHardware (..)
  , module Feldspar.Hardware.Frontend
  ) where

import qualified Language.Embedded.Hardware as Hard

import Data.VirtualContainer
import Feldspar.Representation
import Feldspar.Frontend as Feld
import Feldspar.Hardware.Representation

--------------------------------------------------------------------------------
-- * Frontend for hardware specific operations.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

newtype Sig a = Sig { unSig :: Virtual SmallType Hard.Signal a }

-- | Create an uninitialized signal.
newSig :: Type a => Hardware (Sig a)
newSig = newNamedSig "s"

-- | Create an uninitialized named signal.
newNamedSig :: Type a => String -> Hardware (Sig a)
newNamedSig name = fmap Sig $ mapVirtualA (const $ Hardware $ Hard.newNamedSignal name) virtRep

-- | Create an initialized signal.
initSig :: Type a => Data a -> Hardware (Sig a)
initSig = initNamedSig "s"

-- | Create an initialized named signal.
initNamedSig :: Type a => String -> Data a -> Hardware (Sig a)
initNamedSig name = fmap Sig . mapVirtualA (Hardware . Hard.initNamedSignal name) . sugar

-- | Get the contents of a signal.
getSig :: Type a => Sig a -> Hardware (Data a)
getSig = fmap desugar . mapVirtualA (Hardware . Hard.getSignal) . unSig

-- | Set the contents of a signal.
setSig :: Type a => Sig a -> Data a -> Hardware ()
setSig s = sequence_ . zipListVirtual (\s' a' -> Hardware $ Hard.setSignal s' a') (unSig s) . sugar

-- | Modify the contents of a signal.
modifySig :: Type a => Sig a -> (Data a -> Data a) -> Hardware ()
modifySig s f = setSig s . f =<< unsafeFreezeSig s

-- | Freeze the contents of a signal.
unsafeFreezeSig :: Type a => Sig a -> Hardware (Data a)
unsafeFreezeSig = fmap desugar . mapVirtualA (Hardware . Hard.unsafeFreezeSignal) . unSig

--------------------------------------------------------------------------------

-}
