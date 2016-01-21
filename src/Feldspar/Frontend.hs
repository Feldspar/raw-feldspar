module Feldspar.Frontend where

import Prelude (Integral, error, reverse)
import Prelude.EDSL

import Control.Monad

import Data.Proxy

import System.IO

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Syntactic.TypeRep
import Language.Syntactic.TypeRep

import qualified Control.Monad.Operational.Higher   as H

import Language.Embedded.Imperative.CMD (IxRange, FunArg, Object)
import qualified Language.Embedded.Imperative       as Soft
import qualified Language.Embedded.Imperative.CMD   as Soft

import Language.Embedded.Hardware (Signal)
import qualified Language.Embedded.Hardware as Hard

import Data.VirtualContainer

import Feldspar.Representation

--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** General constructs

-- | Explicit sharing
share :: (Syntax a, Syntax b) => a -> (a -> b) -> b
share = sugarSymTR Let

-- | For loop
forLoop :: Syntax st => Data Length -> st -> (Data Index -> st -> st) -> st
forLoop = sugarSymTR ForLoop

-- | Conditional expression
cond :: Syntax a => Data Bool -> a -> a -> a
cond = sugarSymTR Condition

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
(?) = cond

infixl 1 ?

--------------------------------------------------------------------------------
-- ** Literals

value :: Syntax a => Internal a -> a
value = sugarSymTR . Literal

false :: Data Bool
false = value False

true :: Data Bool
true = value True

--------------------------------------------------------------------------------
-- ** Primitive functions

instance (SmallType a, Num a) => Num (Data a)
  where
    fromInteger = value . fromInteger
    (+)         = sugarSymTR Add
    (-)         = sugarSymTR Sub
    (*)         = sugarSymTR Mul
    negate      = sugarSymTR Neg
    abs    = error "abs not yet defined for Data"
    signum = error "signum not yet defined for Data"

-- | Integral type casting
i2n :: (Integral i, Num n, SmallType i, SmallType n) => Data i -> Data n
i2n = sugarSymTR I2N

not :: Data Bool -> Data Bool
not = sugarSymTR Not

(==) :: SmallType a => Data a -> Data a -> Data Bool
(==) = sugarSymTR Eq

(<) :: SmallType a => Data a -> Data a -> Data Bool
(<) = sugarSymTR Lt

(>) :: SmallType a => Data a -> Data a -> Data Bool
(>) = sugarSymTR Gt

(<=) :: SmallType a => Data a -> Data a -> Data Bool
(<=) = sugarSymTR Le

(>=) :: SmallType a => Data a -> Data a -> Data Bool
(>=) = sugarSymTR Ge

min :: SmallType a => Data a -> Data a -> Data a
min a b = a<=b ? a $ b

max :: SmallType a => Data a -> Data a -> Data a
max a b = a>=b ? a $ b

--------------------------------------------------------------------------------
-- ** Arrays

-- | Index into an array
unsafeArrIx :: forall a . Type a => Arr a -> Data Index -> Data a
unsafeArrIx arr i = desugar $ mapVirtual arrIx $ unArr arr
  where
    arrIx :: SmallType b => Soft.Arr Index b -> Data b
    arrIx arr = sugarSymTR (UnsafeArrIx arr) i

--------------------------------------------------------------------------------
-- ** Syntactic conversion

desugar :: Syntax a => a -> Data (Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Internal a) -> a
sugar = Syntactic.sugar . unData

resugar :: (Syntax a, Syntax b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar

--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** References

-- | Create an uninitialized reference
newRef :: Type a => Program (Ref a)
newRef = fmap Ref $ mapVirtualA (const (Program Soft.newRef)) virtRep

-- | Create an initialized reference
initRef :: forall a . Type a => Data a -> Program (Ref a)
initRef = fmap Ref . mapVirtualA (Program . Soft.initRef) . sugar

-- | Get the contents of a reference
getRef :: Type a => Ref a -> Program (Data a)
getRef = fmap desugar . mapVirtualA (Program . Soft.getRef) . unRef

-- | Set the contents of a reference
setRef :: Type a => Ref a -> Data a -> Program ()
setRef r = sequence_ . zipListVirtual (\r' a' -> Program $ Soft.setRef r' a') (unRef r) . sugar

-- | Modify the contents of reference
modifyRef :: Type a => Ref a -> (Data a -> Data a) -> Program ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
-- as long as the resulting value is alive)
unsafeFreezeRef :: Type a => Ref a -> Program (Data a)
unsafeFreezeRef = fmap desugar . mapVirtualA (Program . Soft.unsafeFreezeRef) . unRef

--------------------------------------------------------------------------------
-- ** Arrays

-- | Create an uninitialized array
newArr :: forall a . Type a => Data Length -> Program (Arr a)
newArr l = fmap Arr $ mapVirtualA (const (Program $ Soft.newArr l)) rep
  where
    rep = virtRep :: VirtualRep SmallType a

-- | Get an element of an array
getArr :: Type a => Data Index -> Arr a -> Program (Data a)
getArr i = fmap desugar . mapVirtualA (Program . Soft.getArr i) . unArr

-- | Set an element of an array
setArr :: forall a . Type a => Data Index -> Data a -> Arr a -> Program ()
setArr i a arr = sequence_ $
    zipListVirtual (\a' arr' -> Program $ Soft.setArr i a' arr') aS (unArr arr)
  where
    aS = sugar a :: Virtual SmallType Data a

--------------------------------------------------------------------------------
-- ** Control flow

-- | Conditional statement.
iff :: Data Bool -> Program () -> Program () -> Program ()
iff c t f = Program $ Soft.iff c (unProgram t) (unProgram f)

-- | Conditional statement that returns an expression.
ifE :: Type a => Data Bool -> Program (Data a) -> Program (Data a) -> Program (Data a)
ifE c t f = do
<<<<<<< 73aac3c446ea8c6db6267d2ec5d8d50279519f6f
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res
    getRef res

-- | For loop
for :: (Integral n, SmallType n)
    => IxRange (Data n)        -- ^ Range
    -> (Data n -> Program ())  -- ^ Loop body
    -> Program ()
for range body = Program $ Imp.for range (unProgram . body)

-- | While loop
while
    :: Program (Data Bool)  -- ^ Continue condition
    -> Program ()           -- ^ Loop body
    -> Program ()
while cont body = Program $ Imp.while (unProgram cont) (unProgram body)

-- | Break out from a loop
break :: Program ()
break = Program Imp.break

-- | Assertion
assert
    :: Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> Program ()
assert cond msg = Program $ Imp.assert cond msg

--------------------------------------------------------------------------------
-- ** Pointer operations

-- | Swap two pointers
--
-- This is generally an unsafe operation. E.g. it can be used to make a
-- reference to a data structure escape the scope of the data.
--
-- The 'IsPointer' class ensures that the operation is only possible for types
-- that are represented as pointers in C.
unsafeSwap :: IsPointer a => a -> a -> Program ()
unsafeSwap a b = Program $ Imp.unsafeSwap a b

--------------------------------------------------------------------------------
-- ** File handling
{-
-- | Open a file
fopen :: FilePath -> IOMode -> Program Handle
fopen file = Program . Imp.fopen file

-- | Close a file
fclose :: Handle -> Program ()
fclose = Program . Imp.fclose

-- | Check for end of file
feof :: Handle -> Program (Data Bool)
feof = Program . Imp.feof

--------------------------------------------------------------------------------
-- * Software.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** File handling.

-- | Open a file.
fopen :: FilePath -> IOMode -> Software Soft.Handle
fopen file = Software . Soft.fopen file

-- | Close a file.
fclose :: Soft.Handle -> Software ()
fclose = Software . Soft.fclose

-- | Check for end of file.
feof :: Soft.Handle -> Software (Data Bool)
feof = Software . Soft.feof

class PrintfType r
  where
    fprf :: Soft.Handle -> String -> [Soft.PrintfArg Data] -> r

instance (a ~ ()) => PrintfType (Software a)
  where
    fprf h form = Software . H.singleE . Soft.FPrintf h form . reverse

instance (Soft.Formattable a, SmallType a, PrintfType r) => PrintfType (Data a -> r)
  where
    fprf h form as = \a -> fprf h form (Soft.PrintfArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Soft.Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle.
fput :: (Soft.Formattable a, SmallType a)
     => Soft.Handle
     -> String      -- Prefix
     -> Data a      -- Expression to print
     -> String      -- Suffix
     -> Software ()
fput h pre e post = Software $ Soft.fput h pre e post

-- | Get a single value from a handle.
fget :: (Soft.Formattable a, SmallType a) => Soft.Handle -> Software (Data a)
fget = Software . Soft.fget

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf Soft.stdout

--------------------------------------------------------------------------------
-- ** External function calls (C-specific)

-- | Add an @#include@ statement to the generated code.
addInclude :: String -> Software ()
addInclude = Software . Soft.addInclude

-- | Add a global definition to the generated code
--
-- Can be used conveniently as follows:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > import Feldspar.IO
-- >
-- > prog = do
-- >     ...
-- >     addDefinition myCFunction
-- >     ...
-- >   where
-- >     myCFunction = [cedecl|
-- >       void my_C_function( ... )
-- >       {
-- >           // C code
-- >           // goes here
-- >       }
-- >       |]
addDefinition :: Soft.Definition -> Software ()
addDefinition = Software . Soft.addDefinition

-- | Declare an external function.
addExternFun :: forall proxy res . SmallType res
    => String         -- ^ Function name
    -> proxy res      -- ^ Proxy for expression and result type
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Software ()
addExternFun fun res args = Software $ Soft.addExternFun fun res' args
  where
    res' = Proxy :: Proxy (Data res)

-- | Declare an external procedure.
addExternProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Software ()
addExternProc proc args = Software $ Soft.addExternProc proc args

-- | Call a function.
callFun :: SmallType a
    => String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Software (Data a)
callFun fun as = Software $ Soft.callFun fun as

-- | Call a procedure.
callProc
    :: String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Software ()
callProc fun as = Software $ Soft.callProc fun as

-- | Declare and call an external function.
externFun :: SmallType res
    => String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Software (Data res)
externFun fun args = Software $ Soft.externFun fun args

-- | Declare and call an external procedure.
externProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Software ()
externProc proc args = Software $ Soft.externProc proc args

-- | Get current time as number of seconds passed today.
getTime :: Software (Data Double)
getTime = Software Soft.getTime

-- | Constant string argument.
strArg :: String -> FunArg Data
strArg = Soft.strArg

-- | Value argument.
valArg :: SmallType a => Data a -> FunArg Data
valArg = Soft.valArg

-- | Reference argument.
refArg :: SmallType a => Ref a -> FunArg Data
refArg (Ref r) = Soft.refArg (viewActual r)

-- | Array argument.
arrArg :: SmallType a => Arr a -> FunArg Data
arrArg (Arr a) = Soft.arrArg (viewActual a)

-- | Abstract object argument.
objArg :: Object -> FunArg Data
objArg = Soft.objArg

-- | Modifier that takes the address of another argument.
addr :: FunArg Data -> FunArg Data
addr = Soft.addr

--------------------------------------------------------------------------------
-- ** Abstract objects

newObject
    :: String  -- ^ Object type
    -> Software Object
newObject = Software . Soft.newObject

initObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Software Object
initObject fun ty args = Software $ Soft.initObject fun ty args

initUObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Software Object
initUObject fun ty args = Software $ Soft.initUObject fun ty args

--------------------------------------------------------------------------------
-- * Hardware
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

newtype Sig a = Sig { unSig :: Virtual SmallType Hard.Signal a }

-- | Create an uninitialized signal.
newSig :: Type a => Hardware (Sig a)
newSig = fmap Sig $ mapVirtualA (const (Hardware Hard.newSignal_)) virtRep

-- | Create an initialized signal.
initSig :: forall a. Type a => Data a -> Hardware (Sig a)
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
-- ** Structural.

data SigX = forall a. SigX (Sig a)

hides :: Sig a -> SigX
hides = SigX

-- | Wraps the program in an entity declaration.
entity :: String -> Hardware a -> Hardware a
entity e body = Hardware $ Hard.entity e (unHardware body)

-- | Wraps the program in an architecture.
architecture :: String -> String -> Hardware a -> Hardware a
architecture e a body = Hardware $ Hard.architecture e a (unHardware body)

-- | Wraps the program in a process.
process :: [SigX] -> Hardware () -> Hardware ()
process xs body = undefined
  -- Hardware $ Hard.process (fmap (\(SigX (Sig s)) -> Hard.hideSig s) xs) (unHardware body)

--------------------------------------------------------------------------------
