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
newSig :: HPrimType a => Hardware (Signal a)
newSig = newNamedSig "s"

-- | Create an uninitialized named signal.
newNamedSig :: HPrimType a => String -> Hardware (Signal a)
newNamedSig name = Hardware $ Hard.newNamedSignal name

-- | Create an initialized signal.
initSig :: HPrimType a => HData a -> Hardware (Signal a)
initSig = Hardware . Hard.initSignal

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
-- Ports.

initNamedPort :: HPrimType a => String -> Hard.Mode -> HData a -> Hardware (Signal a)
initNamedPort s m = Hardware . Hard.initNamedPort s m

initPort :: HPrimType a => Hard.Mode -> HData a -> Hardware (Signal a)
initPort m = Hardware . Hard.initPort m

newNamedPort :: HPrimType a => String -> Hard.Mode -> Hardware (Signal a)
newNamedPort s = Hardware . Hard.newNamedPort s

newPort :: HPrimType a => Hard.Mode -> Hardware (Signal a)
newPort = Hardware . Hard.newPort 

--------------------------------------------------------------------------------
-- Short-hands.

signal :: HPrimType a => String -> Hardware (Signal a)
signal = Hardware . Hard.signal

(<--) :: (Syntax HData a, HPrimType (Internal a)) => Signal (Internal a) -> a -> Hardware ()
(<--) s = setSig s . Syntactic.resugar

(<=-) :: HPrimType a => Signal a -> Signal a -> Hardware ()
(<=-) s v = do v' <- unsafeFreezeSig v; setSig s v'

(<==) :: HPrimType a => Signal a -> HData a -> Hardware ()
(<==) = setSig

--------------------------------------------------------------------------------
-- * Variables.

-- VHDL's variables are implemented using the 'Ref' type from the general
-- frontend.

type Variable = Ref HData

--------------------------------------------------------------------------------
-- Short-hands.

variable :: HType a => String -> Hardware (Variable a)
variable = newNamedRef

(==:) :: HType a => Variable a -> HData a -> Hardware ()
(==:) = setRef

--------------------------------------------------------------------------------
-- ** Constants.

initNamedConst :: HPrimType a => String -> HData a -> Hardware (Constant a)
initNamedConst name = Hardware . Hard.initNamedConstant name

initConst :: HPrimType a => HData a -> Hardware (Constant a)
initConst = Hardware . Hard.initConstant

getConst :: HPrimType a => Constant a -> Hardware (HData a)
getConst = Hardware . Hard.getConstant

--------------------------------------------------------------------------------
-- Short-hands.

constant :: HPrimType a => String -> HData a -> Hardware (Constant a)
constant name = Hardware . Hard.constant name

--------------------------------------------------------------------------------
-- ** Arrays.

getArray :: (HPrimType i, Integral i, Ix i) => HData i -> Signal (Hard.Bits n) -> Hardware (HData Hard.Bit)
getArray ix = Hardware . Hard.getArray ix

setArray :: (HPrimType i, Integral i, Ix i) => HData i -> HData (Hard.Bit) -> Signal (Hard.Bits n) -> Hardware ()
setArray ix e = Hardware . Hard.setArray ix e

----

getSignalRange
  :: (HPrimType i, Integral i, Ix i)
  => HData i
  -> (HData i, HData i)
  -> Signal (Hard.Bits n)
  -> Hardware (HData Hard.UBits)
getSignalRange size range = Hardware . Hard.getSignalRange size range

setSignalRange
  :: (HPrimType i, Integral i, Ix i)
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

type Case a = Hard.When a (Oper.ProgramT HardwareCMD (Oper.Param2 HData HPrimType') (Oper.Program CompCMD (Oper.Param2 HData HPrimType')))

switched :: (HPrimType a, Eq a, Ord a) => HData a -> [Case a] -> Hardware () -> Hardware ()
switched c cs = Hardware . Hard.switched c cs . unHardware

switch :: (HPrimType a, Eq a, Ord a) => HData a -> [Case a] -> Hardware ()
switch c = Hardware . Hard.switch c

is :: HPrimType a => a -> Hardware () -> Case a
is c = Hard.is c . unHardware

to :: HPrimType a => a -> a -> Hardware () -> Case a
to l u = Hard.to l u . unHardware

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

nill :: Hard.Arg ()
nill = Hard.Nill

--------------------------------------------------------------------------------

uniqueInput :: HPrimType a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueInput = Hard.exactInput

input :: HPrimType a => (Signal a -> Signature b) -> Signature (Signal a -> b)
input = Hard.input

uniqueOutput :: HPrimType a => String -> (Signal a -> Signature b) -> Signature (Signal a -> b)
uniqueOutput = Hard.exactOutput

output :: HPrimType a => (Signal a -> Signature b) -> Signature (Signal a -> b)
output = Hard.output

ret :: Hardware () -> Signature ()
ret = Hard.ret . unHardware

--------------------------------------------------------------------------------
-- ** Structural.

process :: [Ident] -> Hardware () -> Hardware ()
process is = Hardware . Hard.process is . unHardware

--------------------------------------------------------------------------------
