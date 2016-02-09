module Feldspar.Frontend where


import Prelude (Integral, error, (=<<), sequence_)
import Prelude.EDSL

import Data.Int

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Syntactic.TypeRep

import Language.Embedded.Imperative (IxRange)
import qualified Language.Embedded.Imperative as Imp

import qualified Data.Inhabited as Inhabited
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

instance Syntactic.Syntactic ()
  where
    type Domain ()   = FeldDomain
    type Internal () = Int32
    desugar () = unData 0
    sugar   _  = ()

-- | Example value
--
-- 'example' can be used similarly to 'undefined' in normal Haskell, i.e. to
-- create an expression whose value is irrelevant.
--
-- Note that it is generally not possible to use 'undefined' in Feldspar
-- expressions, as this will crash the compiler.
example :: Syntax a => a
example = value Inhabited.example



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
arrIx :: Syntax a => IArr (Internal a) -> Data Index -> a
arrIx arr i = resugar $ mapVirtual ix $ unIArr arr
  where
    ix :: SmallType b => Imp.IArr Index b -> Data b
    ix arr = sugarSymTR (ArrIx arr) i



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
class Monad m => MonadComp m
  where
    -- | Lift a 'Comp' computation
    liftComp :: Comp a -> m a
    -- | Conditional statement
    iff :: Data Bool -> m () -> m () -> m ()
    -- | For loop
    for :: (Integral n, SmallType n) => IxRange (Data n) -> (Data n -> m ()) -> m ()
    -- | While loop
    while :: m (Data Bool) -> m () -> m ()

instance MonadComp Comp
  where
    liftComp        = id
    iff c t f       = Comp $ Imp.iff c (unComp t) (unComp f)
    for  range body = Comp $ Imp.for range (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)



----------------------------------------
-- ** References
----------------------------------------

-- | Create an uninitialized reference.
newRef :: (Type a, MonadComp m) => m (Ref a)
newRef = liftComp $ fmap Ref $ mapVirtualA (const (Comp Imp.newRef)) virtRep

-- | Create an initialized reference.
initRef :: (Syntax a, MonadComp m) => a -> m (Ref (Internal a))
initRef = liftComp . fmap Ref . mapVirtualA (Comp . Imp.initRef) . resugar

-- | Get the contents of a reference.
getRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> m a
getRef = liftComp . fmap resugar . mapVirtualA (Comp . Imp.getRef) . unRef

-- | Set the contents of a reference.
setRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> a -> m ()
setRef r
    = liftComp
    . sequence_
    . zipListVirtual (\r' a' -> Comp $ Imp.setRef r' a') (unRef r)
    . resugar

-- | Modify the contents of reference.
modifyRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> (a -> a) -> m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> m a
unsafeFreezeRef
    = liftComp
    . fmap resugar
    . mapVirtualA (Comp . Imp.unsafeFreezeRef)
    . unRef



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Create an uninitialized array
newArr :: forall m a . (Type a, MonadComp m) => Data Length -> m (Arr a)
newArr l = liftComp $ fmap Arr $ mapVirtualA (const (Comp $ Imp.newArr l)) rep
  where
    rep = virtRep :: VirtualRep SmallType a

-- | Get an element of an array
getArr :: (Syntax a, MonadComp m) => Data Index -> Arr (Internal a) -> m a
getArr i = liftComp . fmap resugar . mapVirtualA (Comp . Imp.getArr i) . unArr

-- | Set an element of an array
setArr :: forall m a . (Syntax a, MonadComp m) =>
    Data Index -> a -> Arr (Internal a) -> m ()
setArr i a
    = liftComp
    . sequence_
    . zipListVirtual (\a' arr' -> Comp $ Imp.setArr i a' arr') rep
    . unArr
  where
    rep = resugar a :: Virtual SmallType Data (Internal a)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either
-- array.
copyArr :: (Type a, MonadComp m)
    => Arr a        -- ^ Destination
    -> Arr a        -- ^ Source
    -> Data Length  -- ^ Number of elements
    -> m ()
copyArr arr1 arr2 len = liftComp $ sequence_ $
    zipListVirtual (\a1 a2 -> Comp $ Imp.copyArr a1 a2 len)
      (unArr arr1)
      (unArr arr2)

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (Type a, MonadComp m)
    => Arr a
    -> Data Length  -- ^ Length of array
    -> m (IArr a)
freezeArr arr n
    = liftComp
    $ fmap IArr
    $ mapVirtualA (Comp . flip Imp.freezeArr n)
    $ unArr arr

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: (Type a, MonadComp m) => Arr a -> m (IArr a)
unsafeFreezeArr
    = liftComp
    . fmap IArr
    . mapVirtualA (Comp . Imp.unsafeFreezeArr)
    . unArr



----------------------------------------
-- ** Control-flow
----------------------------------------

-- | Conditional statement that returns an expression
ifE :: (Syntax a, MonadComp m)
    => Data Bool  -- ^ Condition
    -> m a        -- ^ True branch
    -> m a        -- ^ False branch
    -> m a
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res

-- | Break out from a loop
break :: MonadComp m => m ()
break = liftComp $ Comp Imp.break

-- | Assertion
assert :: MonadComp m
    => Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> m ()
assert cond msg = liftComp $ Comp $ Imp.assert cond msg

