module Feldspar.Hardware.Frontend
  ( Hardware
  , MonadHardware(..)

  , Signal
  , Constant
  , Ident, ToIdent, (.:), (.>)
    
  , module Feldspar.Hardware.Frontend
  ) where

import Language.Embedded.Hardware (Signal, Constant, Ident, ToIdent, (.:), (.>))
import qualified Language.Embedded.Hardware as Hard

import Data.Ix (Ix)

import qualified Control.Monad.Operational.Higher as Oper

import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Data.TypedStruct
import Feldspar.Frontend (newNamedRef, setRef)
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()
import Feldspar.Representation
import Feldspar.Hardware.Representation

import GHC.TypeLits (KnownNat)

--------------------------------------------------------------------------------
-- * Frontend for hardware specific operations.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | Create an uninitialized signal.
newSig :: PrimType HData a => Hardware (Signal a)
newSig = newNamedSig "s"

-- | Create an uninitialized named signal.
newNamedSig :: PrimType HData a => String -> Hardware (Signal a)
newNamedSig name = Hardware $ Hard.newNamedSignal name

-- | Create an initialized signal.
initSig :: PrimType HData a => HData a -> Hardware (Signal a)
initSig = Hardware . Hard.initSignal

-- | Create an initialized named signal.
initNamedSig :: PrimType HData a => String -> HData a -> Hardware (Signal a)
initNamedSig name e = Hardware $ Hard.initNamedSignal name e

-- | Get the contents of a signal.
getSig :: PrimType HData a => Signal a -> Hardware (HData a)
getSig = Hardware . Hard.getSignal

-- | Set the contents of a signal.
setSig :: PrimType HData a => Signal a -> HData a -> Hardware ()
setSig s = Hardware . Hard.setSignal s

-- | Modify the contents of a signal.
modifySig :: PrimType HData a => Signal a -> (HData a -> HData a) -> Hardware ()
modifySig s f = setSig s . f =<< unsafeFreezeSig s

-- | Freeze the contents of a signal.
unsafeFreezeSig :: PrimType HData a => Signal a -> Hardware (HData a)
unsafeFreezeSig = Hardware . Hard.unsafeFreezeSignal

--------------------------------------------------------------------------------
-- Ports.

initNamedPort :: PrimType HData a => String -> Hard.Mode -> HData a -> Hardware (Signal a)
initNamedPort s m = Hardware . Hard.initNamedPort s m

initPort :: PrimType HData a => Hard.Mode -> HData a -> Hardware (Signal a)
initPort m = Hardware . Hard.initPort m

newNamedPort :: PrimType HData a => String -> Hard.Mode -> Hardware (Signal a)
newNamedPort s = Hardware . Hard.newNamedPort s

newPort :: PrimType HData a => Hard.Mode -> Hardware (Signal a)
newPort = Hardware . Hard.newPort 

--------------------------------------------------------------------------------
-- Short-hands.

signal :: PrimType HData a => String -> Hardware (Signal a)
signal = Hardware . Hard.signal

(<--) :: (Syntax HData a, PrimType HData (Internal a)) => Signal (Internal a) -> a -> Hardware ()
(<--) s = setSig s . Syntactic.resugar

(<=-) :: PrimType HData a => Signal a -> Signal a -> Hardware ()
(<=-) s v = do v' <- unsafeFreezeSig v; setSig s v'

(<==) :: PrimType HData a => Signal a -> HData a -> Hardware ()
(<==) = setSig

--------------------------------------------------------------------------------
-- * Variables.

-- VHDL's variables are implemented using the 'Ref' type from the general
-- frontend.

type Variable = Ref HData

--------------------------------------------------------------------------------
-- Short-hands.

variable :: Type HData a => String -> Hardware (Variable a)
variable = newNamedRef

(==:) :: Type HData a => Variable a -> HData a -> Hardware ()
(==:) = setRef

--------------------------------------------------------------------------------
-- ** Constants.

initNamedConst :: PrimType HData a => String -> HData a -> Hardware (Constant a)
initNamedConst name = Hardware . Hard.initNamedConstant name

initConst :: PrimType HData a => HData a -> Hardware (Constant a)
initConst = Hardware . Hard.initConstant

getConst :: PrimType HData a => Constant a -> Hardware (HData a)
getConst = Hardware . Hard.getConstant

--------------------------------------------------------------------------------
-- Short-hands.

constant :: PrimType HData a => String -> HData a -> Hardware (Constant a)
constant name = Hardware . Hard.constant name

--------------------------------------------------------------------------------
-- ** Arrays.

getArray :: (PrimType HData i, Integral i, Ix i) => HData i -> Signal (Hard.Bits n) -> Hardware (HData Hard.Bit)
getArray ix = Hardware . Hard.getArray ix

setArray :: (PrimType HData i, Integral i, Ix i) => HData i -> HData (Hard.Bit) -> Signal (Hard.Bits n) -> Hardware ()
setArray ix e = Hardware . Hard.setArray ix e

----

getSignalRange
  :: (PrimType HData i, Integral i, Ix i)
  => HData i
  -> (HData i, HData i)
  -> Signal (Hard.Bits n)
  -> Hardware (HData Hard.UBits)
getSignalRange size range = Hardware . Hard.getSignalRange size range

setSignalRange
  :: (PrimType HData i, Integral i, Ix i)
  => (HData i, HData i)
  -> Signal (Hard.Bits n)
  -> (HData i, HData i)
  -> Signal (Hard.Bits m)
  -> Hardware ()
setSignalRange range1 s1 range2 s2 = Hardware $ Hard.setSignalRange range1 s1 range2 s2

----

asSigned :: KnownNat n => Signal (Hard.Bits n) -> Hardware (HData Integer)
asSigned = Hardware . Hard.asSigned

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- Virtual arrays, or variables of array type, are represented using 'Arr' from
-- the general frontend.

type Array = Arr HData

--------------------------------------------------------------------------------
-- ** Looping.

-- Looping is also covered by the general frontend.

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- Conditional statements are also (mostly) covered by the general frontend.
-- Some constructs, like case statements, are not yet supported by the C side.

type Case a = Hard.When a (Oper.ProgramT HardwareCMD (Oper.Param2 HData (PrimType' HPrim)) (Oper.Program CompCMD (Oper.Param2 HData (PrimType' HPrim))))

switched :: (PrimType HData a, Eq a, Ord a) => HData a -> [Case a] -> Hardware () -> Hardware ()
switched c cs = Hardware . Hard.switched c cs . unHardware

switch :: (PrimType HData a, Eq a, Ord a) => HData a -> [Case a] -> Hardware ()
switch c = Hardware . Hard.switch c

is :: PrimType HData a => a -> Hardware () -> Case a
is c = Hard.is c . unHardware

to :: PrimType HData a => a -> a -> Hardware () -> Case a
to l u = Hard.to l u . unHardware

--------------------------------------------------------------------------------
-- ** Components.

-- | Signature of a hardware component's interface, wrapping a hardware program.
type Signature = Hard.Sig  HardwareCMD HData (PrimType' HPrim) (Oper.Program CompCMD (Oper.Param2 HData (PrimType' HPrim)))

-- | Hardware componenets.
type Component = Hard.Comp HardwareCMD HData (PrimType' HPrim) (Oper.Program CompCMD (Oper.Param2 HData (PrimType' HPrim)))

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

nill :: Hard.Arg ()
nill = Hard.Nill

--------------------------------------------------------------------------------

uniqueInput :: PrimType HData a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueInput = Hard.exactInput

input :: PrimType HData a => (Signal a -> Signature b) -> Signature (Signal a -> b)
input = Hard.input

uniqueOutput :: PrimType HData a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueOutput = Hard.exactOutput

output :: PrimType HData a => (Signal a -> Signature b) -> Signature (Signal a -> b)
output = Hard.output

ret :: Hardware () -> Signature ()
ret = Hard.ret . unHardware

--------------------------------------------------------------------------------
-- ** Structural.

process :: [Ident] -> Hardware () -> Hardware ()
process is = Hardware . Hard.process is . unHardware

--------------------------------------------------------------------------------
