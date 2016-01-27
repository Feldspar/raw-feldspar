module Feldspar.Hardware.Frontend
  ( Hardware
  , liftH
  , module Feldspar.Hardware.Frontend
  ) where



import qualified Language.Embedded.Hardware as Hard

import Data.VirtualContainer
import Feldspar.Representation
import Feldspar.Frontend as Feld
import Feldspar.Hardware.Representation



--------------------------------------------------------------------------------
-- ** Signals.

newtype Sig a = Sig { unSig :: Virtual SmallType Hard.Signal a }

-- | Create an uninitialized signal.
newSig :: Type a => Hardware (Sig a)
newSig = fmap Sig $ mapVirtualA (const (Hardware Hard.newSignal_)) virtRep

-- | Create an initialized signal.
initSig :: Type a => Data a -> Hardware (Sig a)
initSig = fmap Sig . mapVirtualA (Hardware . Hard.newSignal) . sugar

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
-- ** ...

instance References (Hardware)
  where
    newRef          = liftH newRef
    initRef         = liftH . initRef
    getRef          = liftH . getRef
    setRef r        = liftH . setRef r
    modifyRef r     = liftH . modifyRef r
    unsafeFreezeRef = liftH . unsafeFreezeRef

instance Arrays (Hardware)
  where
    newArr        = liftH . newArr
    getArr i      = liftH . getArr i
    setArr i v    = liftH . setArr i v
    copyArr a1 a2 = liftH . copyArr a1 a2

instance Controls (Hardware)
  where
    iff c t f          = Hardware $ Hard.iff c (unHardware t) (unHardware f)
    for (i, _, _) body = Hardware $ Hard.for i (unHardware . body)
    while cont body    = Hardware $ Hard.while (unHardware cont) (unHardware body)
    break              = liftH Feld.break
    assert cond msg    = liftH $ assert cond msg

--------------------------------------------------------------------------------
