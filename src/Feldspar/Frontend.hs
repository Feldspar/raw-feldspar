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

import qualified Control.Monad.Operational.Higher as H
import qualified Language.Embedded.VHDL.Command   as Imp

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
cond :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
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

-- | Literal
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
    arrIx :: SmallType b => Imp.Array Index b -> Data b
    arrIx arr = sugarSymTR (UnsafeArrIx arr) i

--------------------------------------------------------------------------------
-- ** Syntactic conversion

desugar :: Syntax a => a -> Data (Syntactic.Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Syntactic.Internal a) -> a
sugar = Syntactic.sugar . unData

resugar :: (Syntax a, Syntax b, Syntactic.Internal a ~ Syntactic.Internal b) => a -> b
resugar = Syntactic.resugar

--------------------------------------------------------------------------------
-- * Programs
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** References

-- | Create an uninitialized reference
newRef :: Type a => Program (Ref a)
newRef = fmap Ref $ mapVirtualA (const (Program Imp.newVariable_)) virtRep

-- | Create an initialized reference
initRef :: forall a . Type a => Data a -> Program (Ref a)
initRef = fmap Ref . mapVirtualA (Program . Imp.newVariable) . sugar

-- | Get the contents of a reference
getRef :: Type a => Ref a -> Program (Data a)
getRef = fmap desugar . mapVirtualA (Program . Imp.getVariable) . unRef

-- | Set the contents of a reference
setRef :: Type a => Ref a -> Data a -> Program ()
setRef r
    = sequence_
    . zipListVirtual (\r' a' -> Program $ Imp.setVariable r' a') (unRef r)
    . sugar

-- | Modify the contents of reference
modifyRef :: Type a => Ref a -> (Data a -> Data a) -> Program ()
modifyRef r f = setRef r . f =<< getRef r

{-
-- | Freeze the contents of reference (only safe if the reference is not updated
-- as long as the resulting value is alive)
unsafeFreezeRef :: Type a => Ref a -> Program (Data a)
unsafeFreezeRef = fmap desugar . mapVirtualA (Program . Imp.unsafeFreezeRef) . unRef
-}
--------------------------------------------------------------------------------
-- ** Arrays

-- | Create an uninitialized array
newArr :: forall a . Type a => Data Length -> Program (Arr a)
newArr l = fmap Arr $ mapVirtualA (const (Program $ Imp.newArray l)) rep
  where
    rep = virtRep :: VirtualRep SmallType a

-- | Get an element of an array
getArr :: Type a => Data Index -> Arr a -> Program (Data a)
getArr i = fmap desugar . mapVirtualA (Program . Imp.getArray i) . unArr

-- | Set an element of an array
setArr :: forall a . Type a => Data Index -> Data a -> Arr a -> Program ()
setArr i a arr = sequence_ $
    zipListVirtual (\a' arr' -> Program $ Imp.setArray i a' arr') aS (unArr arr)
  where
    aS = sugar a :: Virtual SmallType Data a

--------------------------------------------------------------------------------
-- ** Control flow

-- | Conditional statement
iff
    :: Data Bool   -- ^ Condition
    -> Program ()  -- ^ True branch
    -> Program ()  -- ^ False branch
    -> Program ()
iff c t f = Program $ Imp.iff c (unProgram t) (unProgram f)

-- | Conditional statement that returns an expression
ifE :: Type a
    => Data Bool         -- ^ Condition
    -> Program (Data a)  -- ^ True branch
    -> Program (Data a)  -- ^ False branch
    -> Program (Data a)
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    getRef res

-- | For loop
for :: (Integral n, SmallType n)
    => Data n                  -- ^ Range
    -> (Data n -> Program ())  -- ^ Loop body
    -> Program ()
for range body = Program $ Imp.for range (unProgram . body)

--------------------------------------------------------------------------------
-- ** Abstract objects
{-
newObject
    :: String  -- ^ Object type
    -> Program Object
newObject = Program . Imp.newObject

initObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Program Object
initObject fun ty args = Program $ Imp.initObject fun ty args

initUObject
    :: String        -- ^ Function name
    -> String        -- ^ Object type
    -> [FunArg Data] -- ^ Arguments
    -> Program Object
initUObject fun ty args = Program $ Imp.initUObject fun ty args
-}
--------------------------------------------------------------------------------
-- ** External function calls (C-specific)
{-
-- | Add an @#include@ statement to the generated code
addInclude :: String -> Program ()
addInclude = Program . Imp.addInclude

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
addDefinition :: Definition -> Program ()
addDefinition = Program . Imp.addDefinition

-- | Declare an external function
addExternFun :: forall proxy res . SmallType res
    => String         -- ^ Function name
    -> proxy res      -- ^ Proxy for expression and result type
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternFun fun res args = Program $ Imp.addExternFun fun res' args
  where
    res' = Proxy :: Proxy (Data res)

-- | Declare an external procedure
addExternProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments (only used to determine types)
    -> Program ()
addExternProc proc args = Program $ Imp.addExternProc proc args

-- | Call a function
callFun :: SmallType a
    => String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Program (Data a)
callFun fun as = Program $ Imp.callFun fun as

-- | Call a procedure
callProc
    :: String         -- ^ Function name
    -> [FunArg Data]  -- ^ Arguments
    -> Program ()
callProc fun as = Program $ Imp.callProc fun as

-- | Declare and call an external function
externFun :: SmallType res
    => String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Program (Data res)
externFun fun args = Program $ Imp.externFun fun args

-- | Declare and call an external procedure
externProc
    :: String         -- ^ Procedure name
    -> [FunArg Data]  -- ^ Arguments
    -> Program ()
externProc proc args = Program $ Imp.externProc proc args

-- | Get current time as number of seconds passed today
getTime :: Program (Data Double)
getTime = Program Imp.getTime

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
-}
--------------------------------------------------------------------------------
