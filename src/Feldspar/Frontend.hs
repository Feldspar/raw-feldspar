{-# LANGUAGE InstanceSigs #-}

module Feldspar.Frontend where


import Prelude (Integral, error, (=<<), sequence_)
import Prelude.EDSL

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Syntactic.TypeRep

import Language.Embedded.Imperative (IxRange)
import qualified Language.Embedded.Imperative as Imp

import Data.VirtualContainer
import Feldspar.Representation



--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

----------------------------------------
-- ** General constructs
----------------------------------------

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



----------------------------------------
-- ** Literals
----------------------------------------

-- | Literal
value :: Syntax a => Internal a -> a
value = sugarSymTR . Literal

false :: Data Bool
false = value False

true :: Data Bool
true = value True



----------------------------------------
-- ** Primitive functions
----------------------------------------

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



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Index into an array
unsafeArrIx :: Type a => Arr a -> Data Index -> Data a
unsafeArrIx arr i = desugar $ mapVirtual arrIx $ unArr arr
  where
    arrIx :: SmallType b => Imp.Arr Index b -> Data b
    arrIx arr = sugarSymTR (UnsafeArrIx arr) i



----------------------------------------
-- ** Syntactic conversion
----------------------------------------

desugar :: Syntax a => a -> Data (Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Internal a) -> a
sugar = Syntactic.sugar . unData

resugar :: (Syntax a, Syntax b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar



--------------------------------------------------------------------------------
-- * Programs with computational effects
--------------------------------------------------------------------------------

-- | Monads that support computational effects: mutable data structures and
-- control flow
type MonadComp m = (References m, Arrays m, Controls m)

-- | References.
class Monad m => References m
  where
    -- | Create an uninitialized reference.
    newRef    :: Type a => m (Ref a)
    -- | Create an initialized reference.
    initRef   :: Type a => Data a -> m (Ref a)
    -- | Get the contents of a reference.
    getRef    :: Type a => Ref a -> m (Data a)
    -- | Set the contents of a reference.
    setRef    :: Type a => Ref a -> Data a -> m ()
    -- | Modify the contents of reference.
    modifyRef :: Type a => Ref a -> (Data a -> Data a) -> m ()
    -- | Freeze the contents of reference (only safe if the reference is not updated
    --   as long as the resulting value is alive).
    unsafeFreezeRef :: Type a => Ref a -> m (Data a)

-- | Arrays.
class Monad m => Arrays m
  where
    -- | Create an uninitialized array.
    newArr :: Type a => Data Length -> m (Arr a)
    -- | Get an element of an array.
    getArr :: Type a => Data Index -> Arr a -> m (Data a)
    -- | Set an element of an array.
    setArr :: Type a => Data Index -> Data a -> Arr a -> m ()
    -- | Copy the contents of an array to another array. The number of elements to
    -- copy must not be greater than the number of allocated elements in either
    -- array.
    copyArr :: Type a
        => Arr a        -- ^ Destination
        -> Arr a        -- ^ Source
        -> Data Length  -- ^ Number of elements
        -> m ()

-- | Control flow.
class Monad m => Controls m
  where
    -- | Conditional statement.
    iff :: Data Bool -> m () -> m () -> m ()
    -- | For loop.
    for :: (Integral n, SmallType n) => IxRange (Data n) -> (Data n -> m ()) -> m ()
    -- | While loop.
    while :: m (Data Bool) -> m () -> m ()
    -- | Break out from a loop
    break :: m ()
    -- | Assertion
    assert
        :: Data Bool  -- ^ Expression that should be true
        -> String     -- ^ Message in case of failure
        -> m ()

instance References Comp
  where
    newRef   = fmap Ref $ mapVirtualA (const (Comp Imp.newRef)) virtRep
    initRef  = fmap Ref . mapVirtualA (Comp . Imp.initRef) . sugar
    getRef   = fmap desugar . mapVirtualA (Comp . Imp.getRef) . unRef
    setRef r = sequence_ . zipListVirtual (\r' a' -> Comp $ Imp.setRef r' a') (unRef r) . sugar
    modifyRef r f   = setRef r . f =<< unsafeFreezeRef r
    unsafeFreezeRef = fmap desugar . mapVirtualA (Comp . Imp.unsafeFreezeRef) . unRef

instance Arrays Comp
  where
    newArr :: forall a. Type a => Data Length -> Comp (Arr a)
    newArr l = fmap Arr $ mapVirtualA (const (Comp $ Imp.newArr l)) rep
      where rep = virtRep :: VirtualRep SmallType a

    getArr i = fmap desugar . mapVirtualA (Comp . Imp.getArr i) . unArr

    setArr :: forall a. Type a => Data Index -> Data a -> Arr a -> Comp ()
    setArr i a arr = sequence_ $ zipListVirtual (\a' arr' -> Comp $ Imp.setArr i a' arr') (rep) (unArr arr)
      where rep = sugar a :: Virtual SmallType Data a

    copyArr arr1 arr2 len = sequence_ $
        zipListVirtual (\a1 a2 -> Comp $ Imp.copyArr a1 a2 len)
          (unArr arr1)
          (unArr arr2)

instance Controls Comp
  where
    iff c t f       = Comp $ Imp.iff c (unComp t) (unComp f)
    for  range body = Comp $ Imp.for range (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)
    break           = Comp Imp.break
    assert cond msg = Comp $ Imp.assert cond msg

-- | Conditional statement that returns an expression
ifE :: (References m, Controls m, Type a)
    => Data Bool   -- ^ Condition
    -> m (Data a)  -- ^ True branch
    -> m (Data a)  -- ^ False branch
    -> m (Data a)
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res

