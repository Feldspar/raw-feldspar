-- | Monad for running Feldspar programs

module Feldspar.Run.Frontend
  ( Run
  , MonadRun (..)
  , module Feldspar.Run.Frontend
  , module Language.Embedded.Imperative.Frontend.General
  ) where



import qualified Control.Monad.Operational.Higher as Oper

import Language.Embedded.Imperative.Frontend.General hiding (Ref, Arr, IArr)
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp

import Data.VirtualContainer
import Feldspar.Representation
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

instance (Formattable a, SmallType a, PrintfType r) => PrintfType (Data a -> r)
  where
    fprf h form as = \a -> fprf h form (Imp.PrintfArg a : as)

-- | Print to a handle. Accepts a variable number of arguments.
fprintf :: PrintfType r => Handle -> String -> r
fprintf h format = fprf h format []

-- | Put a single value to a handle
fput :: (Formattable a, SmallType a)
    => Handle
    -> String  -- Prefix
    -> Data a  -- Expression to print
    -> String  -- Suffix
    -> Run ()
fput h pre e post = Run $ Imp.fput h pre e post

-- | Get a single value from a handle
fget :: (Formattable a, SmallType a) => Handle -> Run (Data a)
fget = Run . Imp.fget

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf Imp.stdout



--------------------------------------------------------------------------------
-- * C-specific commands
--------------------------------------------------------------------------------

-- | Create a null pointer
newPtr :: SmallType a => Run (Ptr a)
newPtr = newNamedPtr "p"

-- | Create a named null pointer
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedPtr :: SmallType a
    => String  -- ^ Base name
    -> Run (Ptr a)
newNamedPtr = Run . Imp.newNamedPtr

-- | Cast a pointer to an array
ptrToArr :: SmallType a => Ptr a -> Run (Arr a)
ptrToArr = fmap (Arr . Actual) . Run . Imp.ptrToArr

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

-- | Generate code into another translation unit
inModule :: String -> Run () -> Run ()
inModule mod = Run . Imp.inModule mod . unRun

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
addExternFun :: SmallType res
    => String               -- ^ Function name
    -> proxy res            -- ^ Proxy for expression and result type
    -> [FunArg Data CType]  -- ^ Arguments (only used to determine types)
    -> Run ()
addExternFun fun res args = Run $ Imp.addExternFun fun res args

-- | Declare an external procedure
addExternProc
    :: String               -- ^ Procedure name
    -> [FunArg Data CType]  -- ^ Arguments (only used to determine types)
    -> Run ()
addExternProc proc args = Run $ Imp.addExternProc proc args

-- | Call a function
callFun :: SmallType a
    => String               -- ^ Function name
    -> [FunArg Data CType]  -- ^ Arguments
    -> Run (Data a)
callFun fun as = Run $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String               -- ^ Function name
    -> [FunArg Data CType]  -- ^ Arguments
    -> Run ()
callProc fun as = Run $ Imp.callProc fun as

-- | Call a procedure and assign its result
callProcAssign :: Assignable obj
    => obj                  -- ^ Object to which the result should be assigned
    -> String               -- ^ Procedure name
    -> [FunArg Data CType]  -- ^ Arguments
    -> Run ()
callProcAssign obj fun as = Run $ Imp.callProcAssign obj fun as

-- | Declare and call an external function
externFun :: SmallType res
    => String               -- ^ Procedure name
    -> [FunArg Data CType]  -- ^ Arguments
    -> Run (Data res)
externFun fun args = Run $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String               -- ^ Procedure name
    -> [FunArg Data CType]  -- ^ Arguments
    -> Run ()
externProc proc args = Run $ Imp.externProc proc args

-- | Get current time as number of seconds passed today
getTime :: Run (Data Double)
getTime = Run Imp.getTime

-- | Constant string argument
strArg :: String -> FunArg Data CType
strArg = Imp.strArg

-- | Value argument
valArg :: SmallType a => Data a -> FunArg Data CType
valArg = Imp.valArg

-- | Reference argument
refArg :: SmallType a => Ref a -> FunArg Data CType
refArg (Ref r) = Imp.refArg (viewActual r)

-- | Mutable array argument
arrArg :: SmallType a => Arr a -> FunArg Data CType
arrArg (Arr a) = Imp.arrArg (viewActual a)

-- | Immutable array argument
iarrArg :: SmallType a => IArr a -> FunArg Data CType
iarrArg (IArr a) = Imp.iarrArg (viewActual a)

-- | Abstract object argument
objArg :: Object -> FunArg Data CType
objArg = Imp.objArg

-- | Modifier that takes the address of another argument
addr :: FunArg Data CType -> FunArg Data CType
addr = Imp.addr

-- | Modifier that dereferences another argument
deref :: FunArg Data CType -> FunArg Data CType
deref = Imp.deref

