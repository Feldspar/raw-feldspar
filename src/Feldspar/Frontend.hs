{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Frontend where

import Control.Monad.Identity
import Data.Int
import Data.Proxy

import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Embedded.Imperative (IxRange)
import qualified Language.Embedded.Imperative as Imp

import qualified Data.Inhabited as Inhabited
import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Sugar ()

import Prelude (Integral, Floating(..), RealFrac, error)
import Prelude.EDSL
import qualified Prelude

--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------
{-
-- | Explicit sharing
share :: (Syntax exp a, Syntax exp b)
    => exp a             -- ^ Value to share
    -> (exp a -> exp b)  -- ^ Body in which to share the value
    -> exp b
share = shareTag ""
-}
-- | Explicit tagged sharing
shareTag
  :: forall sup   exp a b.
     ( Syntax exp a
     , Syntax exp b
     , Domain b       ~ (sup :&: TypeRepFun)
     , Domain (exp a) ~ (sup :&: TypeRepFun)
     , Domain (exp b) ~ (sup :&: TypeRepFun)
     , BindingT :<: sup
     , Let :<: sup
     , Syntactic (exp a)
     , Syntactic (exp b)       
     , Type (Internal (exp a))
     , Type (Internal (exp b))
     )
  => String            -- ^ A tag (that may be empty). May be used by a back
                       --   end to generate a sensible variable name.
  -> exp a             -- ^ Value to share
  -> (exp a -> exp b)  -- ^ Body in which to share the value
  -> exp b
shareTag tag = sugarSymExp (Proxy::Proxy (DomainOf exp)) (Let tag)

--------------------------------------------------------------------------------
-- ** General constructs

class For exp st where
  for :: exp Length -> st -> (exp Index -> st -> st) -> st

instance (Syntax Data st) => For Data st where
  for = sugarSymFeld ForLoop

instance (Syntax HData st) => For HData st where
  for = sugarSymHFeld ForLoop

--------------------------------------------------------------------------------

class Cond exp a where
  cond :: exp Bool -> a -> a -> a

instance (Syntax Data a) => Cond Data a where
  cond = sugarSymFeld Cond

instance (Syntax HData a) => Cond HData a where
  cond = sugarSymHFeld Cond

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: Cond exp a
    => exp Bool  -- ^ Condition
    -> a         -- ^ True branch
    -> a         -- ^ False branch
    -> a
(?) = cond

infixl 1 ?

--------------------------------------------------------------------------------
-- ** Literals

class Val (exp :: * -> *) a where
  value :: Internal (exp a) -> exp a

instance (Syntax Data a, Type a) => Val Data a where
  value = sugarSymFeld . Lit

instance (Syntax HData a, Type a) => Val HData a where
  value = sugarSymHFeld . Lit

true :: (Val exp Bool, Internal (exp Bool) ~ Bool) => exp Bool
true = value True

false :: (Val exp Bool, Internal (exp Bool) ~ Bool) => exp Bool
false = value False

--------------------------------------------------------------------------------
-- ** Primitive functions

class NUM (exp :: * -> *) a where
  integer :: Prelude.Integer -> exp a
  plus    :: exp a -> exp a -> exp a
  minus   :: exp a -> exp a -> exp a
  times   :: exp a -> exp a -> exp a
  negate  :: exp a -> exp a

instance (Syntax Data a, PrimType a, Prelude.Num a) => NUM Data a where
  integer = value . fromInteger
  plus    = sugarSymFeld Add
  minus   = sugarSymFeld Sub
  times   = sugarSymFeld Mul
  negate  = sugarSymFeld Neg

instance (Syntax HData a, PrimType a, Prelude.Num a) => NUM HData a where
  integer = value . fromInteger
  plus    = sugarSymHFeld Add
  minus   = sugarSymHFeld Sub
  times   = sugarSymHFeld Mul
  negate  = sugarSymHFeld Neg

--------------------------------------------------------------------------------

class FRAC (exp :: * -> *) a where
  fractional :: Prelude.Rational -> exp a
  divide     :: exp a -> exp a -> exp a

instance (Syntax Data a, PrimType a, Prelude.Fractional a) => FRAC Data a where
  fractional = value . fromRational
  divide     = sugarSymFeld FDiv

instance (Syntax HData a, PrimType a, Prelude.Fractional a) => FRAC HData a where
  fractional = value . fromRational
  divide     = sugarSymHFeld FDiv

--------------------------------------------------------------------------------

class FLOAT (exp :: * -> *) a where
  pi      :: exp a
  power   :: exp a -> exp a -> exp a
  sinus   :: exp a -> exp a
  cosinus :: exp a -> exp a

instance (Syntax Data a, PrimType a, Prelude.Floating a) => FLOAT Data a where
  pi      = sugarSymFeld Pi
  power   = sugarSymFeld Pow
  sinus   = sugarSymFeld Sin
  cosinus = sugarSymFeld Cos

instance (Syntax HData a, PrimType a, Prelude.Floating a) => FLOAT HData a where
  pi      = sugarSymHFeld Pi
  power   = sugarSymHFeld Pow
  sinus   = sugarSymHFeld Sin
  cosinus = sugarSymHFeld Cos

--------------------------------------------------------------------------------

class INTEG (exp :: * -> *) a where
  quotient :: exp a -> exp a -> exp a
  reminder :: exp a -> exp a -> exp a
  round    :: (PrimType n, RealFrac n) => exp n -> exp a
  i2n      :: (PrimType n, Num n)      => exp a -> exp n
  i2b      :: exp a -> exp Bool
  b2i      :: exp Bool -> exp a

instance (Syntax Data a, PrimType a, Prelude.Integral a) => INTEG Data a where
  quotient = sugarSymFeld Quot
  reminder = sugarSymFeld Rem
  round    = sugarSymFeld Round
  i2n      = sugarSymFeld I2N
  i2b      = sugarSymFeld I2B
  b2i      = sugarSymFeld B2I

instance (Syntax HData a, PrimType a, Prelude.Integral a) => INTEG HData a where
  quotient = sugarSymHFeld Quot
  reminder = sugarSymHFeld Rem
  round    = sugarSymHFeld Round
  i2n      = sugarSymHFeld I2N
  i2b      = sugarSymHFeld I2B
  b2i      = sugarSymHFeld B2I

-- | Simultaneous @quot@ and @rem@
quotRem :: (NUM exp a, INTEG exp a, PrimType a) => exp a -> exp a -> (exp a, exp a)
quotRem a b = (q, r)
  where
    q = quotient a b
    r = a `minus` (b `times` q)

--------------------------------------------------------------------------------

class BOOL (exp :: * -> *) where
  not :: exp Bool -> exp Bool
  and :: exp Bool -> exp Bool -> exp Bool
  or  :: exp Bool -> exp Bool -> exp Bool

instance Syntax Data Bool => BOOL Data where
  not = sugarSymFeld Not
  and = sugarSymFeld And
  or  = sugarSymFeld Or

instance Syntax HData Bool => BOOL HData where
  not = sugarSymHFeld Not
  and = sugarSymHFeld And
  or  = sugarSymHFeld Or

--------------------------------------------------------------------------------

class EQ (exp :: * -> *) a where
  eq  :: exp a -> exp a -> exp Bool
  neq :: exp a -> exp a -> exp Bool

instance (Syntax Data a, PrimType a, Prelude.Eq a) => EQ Data a where
  eq  = sugarSymFeld Eq
  neq = sugarSymFeld NEq

instance (Syntax HData a, PrimType a, Prelude.Eq a) => EQ HData a where
  eq  = sugarSymHFeld Eq
  neq = sugarSymHFeld NEq

--------------------------------------------------------------------------------

class ORD (exp :: * -> *) a where
  lt  :: exp a -> exp a -> exp Bool
  lte :: exp a -> exp a -> exp Bool
  gt  :: exp a -> exp a -> exp Bool
  gte :: exp a -> exp a -> exp Bool

instance (Syntax Data a, PrimType a, Prelude.Ord a) => ORD Data a where
  lt  = sugarSymFeld Lt
  lte = sugarSymFeld Le
  gt  = sugarSymFeld Gt
  gte = sugarSymFeld Ge

instance (Syntax HData a, PrimType a, Prelude.Ord a) => ORD HData a where
  lt  = sugarSymHFeld Lt
  lte = sugarSymHFeld Le
  gt  = sugarSymHFeld Gt
  gte = sugarSymHFeld Ge

-- | Return the smallest of two values
min :: (Cond exp (exp a), ORD exp a) => exp a -> exp a -> exp a
min a b = (a `lte` b) ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>

-- | Return the greatest of two values
max :: (Cond exp (exp a), ORD exp a) => exp a -> exp a -> exp a
max a b = (a `gte` b) ? a $ b

--------------------------------------------------------------------------------
-- ** Arrays
{-
-- | Index into an array
arrIx :: Syntax a => IArr (Internal a) -> Data Index -> a
arrIx arr i = resugar $ mapStruct ix $ unIArr arr
  where
    ix :: PrimType' b => Imp.IArr Index b -> Data b
    ix arr = sugarSymFeldPrim (ArrIx arr) i
-}


--------------------------------------------------------------------------------
-- ** Syntactic conversion
{-
desugar :: Syntax a => a -> Data (Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Internal a) -> a
sugar = Syntactic.sugar . unData

-- | Cast between two values that have the same syntactic representation
resugar :: (Syntax a, Syntax b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar
-}
--------------------------------------------------------------------------------
-- * Programs with computational effects
--------------------------------------------------------------------------------
{-
-- | Monads that support computational effects: mutable data structures and
-- control flow
class Monad m => MonadComp exp m
  where
    -- | Lift a 'Comp' computation
    liftComp :: Comp exp a -> m a
    -- | Conditional statement
    iff :: exp Bool -> m () -> m () -> m ()
    -- | For loop
    for :: (Integral n, PrimType n) => IxRange (exp n) -> (exp n -> m ()) -> m ()
    -- | While loop
    while :: m (exp Bool) -> m () -> m ()

instance MonadComp Comp
  where
    liftComp        = id
    iff c t f       = Comp $ Imp.iff c (unComp t) (unComp f)
    for range body  = Comp $ Imp.for range (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)
-}
--------------------------------------------------------------------------------
-- ** References
{-
-- | Create an uninitialized reference
newRef :: (Type a, MonadComp m) => m (Ref a)
newRef = newNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: (Type a, MonadComp m)
    => String  -- ^ Base name
    -> m (Ref a)
newNamedRef base = liftComp $ fmap Ref $
    mapStructA (const $ Comp $ Imp.newNamedRef base) typeRep

-- | Create an initialized named reference
initRef :: (Syntax a, MonadComp m) => a -> m (Ref (Internal a))
initRef = initNamedRef "r"

-- | Create an initialized reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedRef :: (Syntax a, MonadComp m)
    => String  -- ^ Base name
    -> a       -- ^ Initial value
    -> m (Ref (Internal a))
initNamedRef base =
    liftComp . fmap Ref . mapStructA (Comp . Imp.initNamedRef base) . resugar

-- | Get the contents of a reference.
getRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> m a
getRef = liftComp . fmap resugar . mapStructA (Comp . Imp.getRef) . unRef

-- | Set the contents of a reference.
setRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> a -> m ()
setRef r
    = liftComp
    . sequence_
    . zipListStruct (\r' a' -> Comp $ Imp.setRef r' a') (unRef r)
    . resugar

-- | Modify the contents of reference.
modifyRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> (a -> a) -> m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | A version of 'modifyRef' that fixes the value type to @`Data` a@
modifyRefD :: (Type a, MonadComp m) => Ref a -> (Data a -> Data a) -> m ()
modifyRefD r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> m a
unsafeFreezeRef
    = liftComp
    . fmap resugar
    . mapStructA (Comp . Imp.unsafeFreezeRef)
    . unRef
-}
--------------------------------------------------------------------------------
-- ** Arrays
{-
-- | Create an uninitialized array
newArr :: (Type a, MonadComp m) => Data Length -> m (Arr a)
newArr = newNamedArr "a"

-- | Create an uninitialized named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr :: (Type a, MonadComp m)
    => String  -- ^ Base name
    -> Data Length
    -> m (Arr a)
newNamedArr base l = liftComp $ fmap Arr $
    mapStructA (const (Comp $ Imp.newNamedArr base l)) typeRep

-- | Create and initialize an array
initArr :: (PrimType a, MonadComp m)
    => [a]  -- ^ Initial contents
    -> m (Arr a)
initArr = initNamedArr "a"

-- It would seem

-- | Create and initialize a named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (PrimType a, MonadComp m)
    => String  -- ^ Base name
    -> [a]     -- ^ Initial contents
    -> m (Arr a)
initNamedArr base =
    liftComp . fmap (Arr . Single) . Comp . Imp.initNamedArr base

-- | Get an element of an array
getArr :: (Syntax a, MonadComp m) => Data Index -> Arr (Internal a) -> m a
getArr i = liftComp . fmap resugar . mapStructA (Comp . Imp.getArr i) . unArr

-- | Set an element of an array
setArr :: forall m a . (Syntax a, MonadComp m) =>
    Data Index -> a -> Arr (Internal a) -> m ()
setArr i a
    = liftComp
    . sequence_
    . zipListStruct (\a' arr' -> Comp $ Imp.setArr i a' arr') rep
    . unArr
  where
    rep = resugar a :: Struct PrimType' Data (Internal a)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either
-- array.
copyArr :: (Type a, MonadComp m)
    => Arr a        -- ^ Destination
    -> Arr a        -- ^ Source
    -> Data Length  -- ^ Number of elements
    -> m ()
copyArr arr1 arr2 len = liftComp $ sequence_ $
    zipListStruct (\a1 a2 -> Comp $ Imp.copyArr a1 a2 len)
      (unArr arr1)
      (unArr arr2)

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (Type a, MonadComp m)
    => Arr a
    -> Data Length  -- ^ Length of new array
    -> m (IArr a)
freezeArr arr n
    = liftComp
    $ fmap IArr
    $ mapStructA (Comp . flip Imp.freezeArr n)
    $ unArr arr

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: (Type a, MonadComp m) => Arr a -> m (IArr a)
unsafeFreezeArr
    = liftComp
    . fmap IArr
    . mapStructA (Comp . Imp.unsafeFreezeArr)
    . unArr

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr :: (Type a, MonadComp m)
    => IArr a
    -> Data Length  -- ^ Number of elements to copy
    -> m (Arr a)
thawArr arr n
    = liftComp
    $ fmap Arr
    $ mapStructA (Comp . flip Imp.thawArr n)
    $ unIArr
    $ arr

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr :: (Type a, MonadComp m) => IArr a -> m (Arr a)
unsafeThawArr
    = liftComp
    . fmap Arr
    . mapStructA (Comp . Imp.unsafeThawArr)
    . unIArr

-- | Create and initialize an immutable array
initIArr :: (PrimType a, MonadComp m) => [a] -> m (IArr a)
initIArr = liftComp . fmap (IArr . Single) . Comp . Imp.initIArr
-}
--------------------------------------------------------------------------------
-- ** Control-flow
{-
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
-}
--------------------------------------------------------------------------------
