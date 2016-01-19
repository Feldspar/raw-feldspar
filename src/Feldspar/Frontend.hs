module Feldspar.Frontend where

import Prelude (Integral, error, reverse)
import Prelude.EDSL

import Control.Monad

import Data.Proxy

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Syntactic.TypeRep
import Language.Syntactic.TypeRep

import qualified Control.Monad.Operational.Higher   as H

import Language.Embedded.Imperative.CMD (IxRange, FunArg, Object)
import qualified Language.Embedded.Imperative       as Soft
import qualified Language.Embedded.Imperative.CMD   as Soft

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
  res <- newRef
  iff c (t >>= setRef res) (f >>= setRef res)
  getRef res

-- | For loop.
for :: (Integral n, SmallType n) => IxRange (Data n) -> (Data n -> Program ()) -> Program ()
for range body = Program $ Soft.for range (unProgram . body)

-- | While loop.
while :: Program (Data Bool) -> Program () -> Program ()
while cont body = Program $ Soft.while (unProgram cont) (unProgram body)

--------------------------------------------------------------------------------
-- * Software.
--------------------------------------------------------------------------------

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
