-- | System interaction in software

module Feldspar.Software.Frontend
  ( Software
  , liftS
  , module Feldspar.Software.Frontend
  , module Language.Embedded.Imperative.Frontend.General
  ) where



import Data.Proxy

import qualified Control.Monad.Operational.Higher as Oper

import Language.Embedded.Imperative.Frontend.General hiding (Ref, Arr)
import qualified Language.Embedded.Imperative as Imp
import qualified Language.Embedded.Imperative.CMD as Imp

import Data.VirtualContainer
import Feldspar.Representation
import Feldspar.Frontend
import Feldspar.Software.Representation



instance References Software
  where
    newRef          = liftS newRef
    initRef         = liftS . initRef
    getRef          = liftS . getRef
    setRef r        = liftS . setRef r
    modifyRef r     = liftS . modifyRef r
    unsafeFreezeRef = liftS . unsafeFreezeRef

instance Arrays Software
  where
    newArr        = liftS . newArr
    getArr i      = liftS . getArr i
    setArr i v    = liftS . setArr i v
    copyArr a1 a2 = liftS . copyArr a1 a2

instance Controls Software
  where
    iff c t f       = Software $ Imp.iff c (unSoftware t) (unSoftware f)
    for  range body = Software $ Imp.for range (unSoftware . body)
    while cont body = Software $ Imp.while (unSoftware cont) (unSoftware body)
    break           = Software Imp.break
    assert cond msg = Software $ Imp.assert cond msg



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
unsafeSwap :: IsPointer a => a -> a -> Software ()
unsafeSwap a b = Software $ Imp.unsafeSwap a b



--------------------------------------------------------------------------------
-- * File handling
--------------------------------------------------------------------------------

-- | Open a file
fopen :: FilePath -> IOMode -> Software Handle
fopen file = Software . Imp.fopen file

-- | Close a file
fclose :: Handle -> Software ()
fclose = Software . Imp.fclose

-- | Check for end of file
feof :: Handle -> Software (Data Bool)
feof = Software . Imp.feof

class PrintfType r
  where
    fprf :: Handle -> String -> [Imp.PrintfArg Data] -> r

instance (a ~ ()) => PrintfType (Software a)
  where
    fprf h form = Software . Oper.singleE . Imp.FPrintf h form . reverse

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
    -> Software ()
fput h pre e post = Software $ Imp.fput h pre e post

-- | Get a single value from a handle
fget :: (Formattable a, SmallType a) => Handle -> Software (Data a)
fget = Software . Imp.fget

-- | Print to @stdout@. Accepts a variable number of arguments.
printf :: PrintfType r => String -> r
printf = fprintf Imp.stdout



--------------------------------------------------------------------------------
-- * Abstract objects
--------------------------------------------------------------------------------

newObject
    :: String  -- ^ Object type
    -> Software Object
newObject = Software . Imp.newObject

initObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Software Object
initObject fun ty args = Software $ Imp.initObject fun ty args

initUObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Software Object
initUObject fun ty args = Software $ Imp.initUObject fun ty args



--------------------------------------------------------------------------------
-- * External function calls (C-specific)
--------------------------------------------------------------------------------

-- | Add an @#include@ statement to the generated code
addInclude :: String -> Software ()
addInclude = Software . Imp.addInclude

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
addDefinition :: Imp.Definition -> Software ()
addDefinition = Software . Imp.addDefinition

-- | Declare an external function
addExternFun :: forall proxy res . SmallType res
    => String         -- ^ Function name
    -> proxy res      -- ^ Proxy for expression and result type
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Software ()
addExternFun fun res args = Software $ Imp.addExternFun fun res' args
  where
    res' = Proxy :: Proxy (Data res)

-- | Declare an external procedure
addExternProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Software ()
addExternProc proc args = Software $ Imp.addExternProc proc args

-- | Call a function
callFun :: SmallType a
    => String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Software (Data a)
callFun fun as = Software $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Software ()
callProc fun as = Software $ Imp.callProc fun as

-- | Declare and call an external function
externFun :: SmallType res
    => String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Software (Data res)
externFun fun args = Software $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Software ()
externProc proc args = Software $ Imp.externProc proc args

-- | Get current time as number of seconds passed today
getTime :: Software (Data Double)
getTime = Software Imp.getTime

-- | Constant string argument
strArg :: String -> FunArg Data
strArg = Imp.strArg

-- | Value argument
valArg :: SmallType a => Data a -> FunArg Data
valArg = Imp.valArg

-- | Reference argument
refArg :: SmallType a => Ref a -> FunArg Data
refArg (Ref r) = Imp.refArg (viewActual r)

-- | Array argument
arrArg :: SmallType a => Arr a -> FunArg Data
arrArg (Arr a) = Imp.arrArg (viewActual a)

-- | Abstract object argument
objArg :: Object -> FunArg Data
objArg = Imp.objArg

-- | Modifier that takes the address of another argument
addr :: FunArg Data -> FunArg Data
addr = Imp.addr

