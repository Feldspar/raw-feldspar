-- | Monad for running Feldspar programs

module Feldspar.Run.Frontend
  ( Run
  , MonadRun (..)
  , module Feldspar.Run.Frontend
  , module Language.Embedded.Imperative.Frontend.General
  ) where



import Data.Word

import qualified Control.Monad.Operational.Higher as Oper

import Language.Embedded.Imperative.Frontend.General hiding (Ref, Arr, IArr)
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()
import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Run.Representation



--------------------------------------------------------------------------------
-- * Pointer operations
--------------------------------------------------------------------------------

-- | Swap two pointers
--
-- This is generally an unsafe operation. E.g. it can be used to make a
-- reference to a data structure escape the scope of the data.
--
-- The 'IsPointer' class ensures that the operation is only possible for types
-- that are represented as pointers in C.
unsafeSwap :: IsPointer a => a -> a -> Run ()
unsafeSwap a b = Run $ Imp.unsafeSwap a b



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | Open a file
fopen :: FilePath -> IOMode -> Run Handle
fopen file = Run . Imp.fopen file

-- | Close a file
fclose :: Handle -> Run ()
fclose = Run . Imp.fclose

-- | Check for end of file
feof :: Handle -> Run (Data Bool)
feof = Run . Imp.feof

class PrintfType r
  where
    fprf :: Handle -> String -> [Imp.PrintfArg Data] -> r

instance (a ~ ()) => PrintfType (Run a)
  where
    fprf h form = Run . Oper.singleInj . Imp.FPrintf h form . reverse

instance (Formattable a, PrimType a, PrintfType r) => PrintfType (Data a -> r)
  where
    fprf h form as = \a -> fprf h form (Imp.PrintfArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: (Formattable a, PrimType a)
    => Handle
    -> String  -- Prefix
    -> Data a  -- Expression to print
    -> String  -- Suffix
    -> Run ()
fput h pre e post = Run $ Imp.fput h pre e post

-- | Get a single value from a handle
fget :: (Formattable a, PrimType a) => Handle -> Run (Data a)
fget = Run . Imp.fget

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf Imp.stdout



--------------------------------------------------------------------------------
-- * C-specific commands
--------------------------------------------------------------------------------

-- | Create a null pointer
newPtr :: PrimType a => Run (Ptr a)
newPtr = newNamedPtr "p"

-- | Create a named null pointer
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedPtr :: PrimType a
    => String  -- ^ Base name
    -> Run (Ptr a)
newNamedPtr = Run . Imp.newNamedPtr

-- | Cast a pointer to an array
ptrToArr :: PrimType a => Ptr a -> Data Length -> Run (Arr a)
ptrToArr ptr len = do
    status <- initNamedRef "arrstatus" (0 :: Data Word8)
    fmap (Arr 0 len status . Single) $ Run $ Imp.ptrToArr ptr

-- | Create a pointer to an abstract object. The only thing one can do with such
-- objects is to pass them to 'callFun' or 'callProc'.
newObject
    :: String  -- ^ Object type
    -> Bool    -- ^ Pointed?
    -> Run Object
newObject = newNamedObject "obj"

-- | Create a pointer to an abstract object. The only thing one can do with such
-- objects is to pass them to 'callFun' or 'callProc'.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedObject
    :: String  -- ^ Base name
    -> String  -- ^ Object type
    -> Bool    -- ^ Pointed?
    -> Run Object
newNamedObject base t p = Run $ Imp.newNamedObject base t p

-- | Add an @#include@ statement to the generated code
addInclude :: String -> Run ()
addInclude = Run . Imp.addInclude

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
addDefinition :: Imp.Definition -> Run ()
addDefinition = Run . Imp.addDefinition

-- | Declare an external function
addExternFun :: PrimType res
    => String                   -- ^ Function name
    -> proxy res                -- ^ Proxy for expression and result type
    -> [FunArg Data PrimType']  -- ^ Arguments (only used to determine types)
    -> Run ()
addExternFun fun res args = Run $ Imp.addExternFun fun res args

-- | Declare an external procedure
addExternProc
    :: String                   -- ^ Procedure name
    -> [FunArg Data PrimType']  -- ^ Arguments (only used to determine types)
    -> Run ()
addExternProc proc args = Run $ Imp.addExternProc proc args

-- | Call a function
callFun :: PrimType a
    => String                   -- ^ Function name
    -> [FunArg Data PrimType']  -- ^ Arguments
    -> Run (Data a)
callFun fun as = Run $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String                   -- ^ Function name
    -> [FunArg Data PrimType']  -- ^ Arguments
    -> Run ()
callProc fun as = Run $ Imp.callProc fun as

-- | Call a procedure and assign its result
callProcAssign :: Assignable obj
    => obj                      -- ^ Object to which the result should be assigned
    -> String                   -- ^ Procedure name
    -> [FunArg Data PrimType']  -- ^ Arguments
    -> Run ()
callProcAssign obj fun as = Run $ Imp.callProcAssign obj fun as

-- | Declare and call an external function
externFun :: PrimType res
    => String                   -- ^ Procedure name
    -> [FunArg Data PrimType']  -- ^ Arguments
    -> Run (Data res)
externFun fun args = Run $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String                   -- ^ Procedure name
    -> [FunArg Data PrimType']  -- ^ Arguments
    -> Run ()
externProc proc args = Run $ Imp.externProc proc args

-- | Generate code into another translation unit
inModule :: String -> Run () -> Run ()
inModule mod = Run . Imp.inModule mod . unRun

-- | Get current time as number of seconds passed today
getTime :: Run (Data Double)
getTime = Run Imp.getTime

-- | Constant string argument
strArg :: String -> FunArg Data PrimType'
strArg = Imp.strArg

-- | Value argument
valArg :: PrimType' a => Data a -> FunArg Data PrimType'
valArg = Imp.valArg

-- | Reference argument
refArg :: PrimType' a => Ref a -> FunArg Data PrimType'
refArg (Ref r) = Imp.refArg (extractSingle r)

-- | Mutable array argument
arrArg :: PrimType' a => Arr a -> FunArg Data PrimType'
arrArg (Arr o _ _ a) = Imp.offset (Imp.arrArg (extractSingle a)) o
  -- TODO Check if the array is alive. How?

-- | Immutable array argument
iarrArg :: PrimType' a => IArr a -> FunArg Data PrimType'
iarrArg (IArr o _ _ a) = Imp.offset (Imp.iarrArg (extractSingle a)) o
  -- TODO Check if the array is alive. How?

-- | Abstract object argument
objArg :: Object -> FunArg Data PrimType'
objArg = Imp.objArg

-- | Named constant argument
constArg
    :: String  -- ^ Type
    -> String  -- ^ Named constant
    -> FunArg Data PrimType'
constArg = Imp.constArg

-- | Modifier that takes the address of another argument
addr :: FunArg Data PrimType' -> FunArg Data PrimType'
addr = Imp.addr

-- | Modifier that dereferences another argument
deref :: FunArg Data PrimType' -> FunArg Data PrimType'
deref = Imp.deref

