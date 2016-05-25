{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

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

class LET exp where
  shareTag :: (Type a, Type b) => String -> exp a -> (exp a -> exp b) -> exp b

instance LET Data where
  shareTag tag = sugarSymFeld (Let tag)

instance LET HData where
  shareTag tag = sugarSymHFeld (Let tag)

-- | Explicit sharing
share
  :: ( LET exp
     , Syntax exp a
     , Type a
     , Type b)
  => exp a             -- ^ Value to share
  -> (exp a -> exp b)  -- ^ Body in which to share the value
  -> exp b
share = shareTag ""

--------------------------------------------------------------------------------
-- ** General constructs

class FOR exp where
  forLoop :: Syntax exp st => exp Length -> st -> (exp Index -> st -> st) -> st

instance FOR Data where
  forLoop = sugarSymFeld ForLoop

instance FOR HData where
  forLoop = sugarSymHFeld ForLoop

--------------------------------------------------------------------------------

class COND exp where
  cond :: Syntax exp a => exp Bool -> a -> a -> a

instance COND Data where
  cond = sugarSymFeld Cond

instance COND HData where
  cond = sugarSymHFeld Cond

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: (COND exp, Syntax exp a)
    => exp Bool  -- ^ Condition
    -> a         -- ^ True branch
    -> a         -- ^ False branch
    -> a
(?) = cond

infixl 1 ?

--------------------------------------------------------------------------------
-- ** Literals

value
  :: forall sup exp a.
     ( Syntax exp a
       -- new.
     , Domain a ~ (sup :&: TypeRepFun)
     , Primitive :<: sup
       -- ...
     )
  => Internal a -> a
value = sugarSymExp (Proxy::Proxy (Domain a)) . Lit

-- | Example value
--
-- 'example' can be used similarly to 'undefined' in normal Haskell, i.e. to
-- create an expression whose value is irrelevant.
--
-- Note that it is generally not possible to use 'undefined' in Feldspar
-- expressions, as this will crash the compiler.
example
  :: ( Syntax exp a
     , Type (Internal a)
     , Domain a ~ (sup :&: TypeRepFun)
     , Primitive :<: sup)
  => a
example = value Inhabited.example

--------------------------------------------------------------------------------
-- ** Primitive functions

class NUM (exp :: * -> *) where
  integer :: (PrimType a, Prelude.Num a) => Prelude.Integer -> exp a
  plus    :: (PrimType a, Prelude.Num a) => exp a -> exp a -> exp a
  minus   :: (PrimType a, Prelude.Num a) => exp a -> exp a -> exp a
  times   :: (PrimType a, Prelude.Num a) => exp a -> exp a -> exp a
  negate  :: (PrimType a, Prelude.Num a) => exp a -> exp a

instance NUM Data where
  integer = value . fromInteger
  plus    = sugarSymFeld Add
  minus   = sugarSymFeld Sub
  times   = sugarSymFeld Mul
  negate  = sugarSymFeld Neg

instance NUM HData where
  integer = value . fromInteger
  plus    = sugarSymHFeld Add
  minus   = sugarSymHFeld Sub
  times   = sugarSymHFeld Mul
  negate  = sugarSymHFeld Neg

--------------------------------------------------------------------------------

class FRAC (exp :: * -> *) where
  fractional :: (PrimType a, Prelude.Fractional a) => Prelude.Rational -> exp a
  divide     :: (PrimType a, Prelude.Fractional a) => exp a -> exp a -> exp a

instance FRAC Data where
  fractional = value . fromRational
  divide     = sugarSymFeld FDiv

instance FRAC HData where
  fractional = value . fromRational
  divide     = sugarSymHFeld FDiv

--------------------------------------------------------------------------------

class FLOAT (exp :: * -> *) where
  pi      :: (PrimType a, Prelude.Floating a) => exp a
  power   :: (PrimType a, Prelude.Floating a) => exp a -> exp a -> exp a
  sinus   :: (PrimType a, Prelude.Floating a) => exp a -> exp a
  cosinus :: (PrimType a, Prelude.Floating a) => exp a -> exp a

instance FLOAT Data where
  pi      = sugarSymFeld Pi
  power   = sugarSymFeld Pow
  sinus   = sugarSymFeld Sin
  cosinus = sugarSymFeld Cos

instance FLOAT HData where
  pi      = sugarSymHFeld Pi
  power   = sugarSymHFeld Pow
  sinus   = sugarSymHFeld Sin
  cosinus = sugarSymHFeld Cos

--------------------------------------------------------------------------------

class INTEG (exp :: * -> *) where
  quotient :: (PrimType a, Prelude.Integral a) => exp a -> exp a -> exp a
  reminder :: (PrimType a, Prelude.Integral a) => exp a -> exp a -> exp a
  round    :: (PrimType a, Prelude.Integral a, PrimType n, Prelude.RealFrac n) => exp n -> exp a
  i2n      :: (PrimType a, Prelude.Integral a, PrimType n, Prelude.Num n)      => exp a -> exp n
  i2b      :: (PrimType a, Prelude.Integral a) => exp a -> exp Bool
  b2i      :: (PrimType a, Prelude.Integral a) => exp Bool -> exp a

instance INTEG Data where
  quotient = sugarSymFeld Quot
  reminder = sugarSymFeld Rem
  round    = sugarSymFeld Round
  i2n      = sugarSymFeld I2N
  i2b      = sugarSymFeld I2B
  b2i      = sugarSymFeld B2I

instance INTEG HData where
  quotient = sugarSymHFeld Quot
  reminder = sugarSymHFeld Rem
  round    = sugarSymHFeld Round
  i2n      = sugarSymHFeld I2N
  i2b      = sugarSymHFeld I2B
  b2i      = sugarSymHFeld B2I

-- | Simultaneous @quot@ and @rem@
quotRem :: (NUM exp, INTEG exp, PrimType a, Prelude.Integral a) => exp a -> exp a -> (exp a, exp a)
quotRem a b = (q, r)
  where
    q = quotient a b
    r = a `minus` (b `times` q)

--------------------------------------------------------------------------------

class BOOL (exp :: * -> *) where
  true  :: exp Bool
  false :: exp Bool
  not :: exp Bool -> exp Bool
  and :: exp Bool -> exp Bool -> exp Bool
  or  :: exp Bool -> exp Bool -> exp Bool

instance BOOL Data where
  true  = value True
  false = value False
  not = sugarSymFeld Not
  and = sugarSymFeld And
  or  = sugarSymFeld Or

instance BOOL HData where
  true  = value True
  false = value False
  not = sugarSymHFeld Not
  and = sugarSymHFeld And
  or  = sugarSymHFeld Or

infixr 3 &&
infixr 2 ||

(&&) :: BOOL exp => exp Bool -> exp Bool -> exp Bool
(&&) = and

(||) :: BOOL exp => exp Bool -> exp Bool -> exp Bool
(||) = or

--------------------------------------------------------------------------------

class EQ (exp :: * -> *) where
  eq  :: (PrimType a, Prelude.Eq a) => exp a -> exp a -> exp Bool
  neq :: (PrimType a, Prelude.Eq a) => exp a -> exp a -> exp Bool

instance EQ Data where
  eq  = sugarSymFeld Eq
  neq = sugarSymFeld NEq

instance EQ HData where
  eq  = sugarSymHFeld Eq
  neq = sugarSymHFeld NEq

--------------------------------------------------------------------------------

class ORD (exp :: * -> *) where
  lt  :: (PrimType a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  lte :: (PrimType a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  gt  :: (PrimType a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  gte :: (PrimType a, Prelude.Ord a) => exp a -> exp a -> exp Bool

instance ORD Data where
  lt  = sugarSymFeld Lt
  lte = sugarSymFeld Le
  gt  = sugarSymFeld Gt
  gte = sugarSymFeld Ge

instance ORD HData where
  lt  = sugarSymHFeld Lt
  lte = sugarSymHFeld Le
  gt  = sugarSymHFeld Gt
  gte = sugarSymHFeld Ge

-- | Return the smallest of two values
min :: (COND exp, ORD exp, Type a, PrimType' a, Syntax exp (exp a))
    => exp a -> exp a -> exp a
min a b = (a `lte` b) ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>)

-- | Return the greatest of two values
max :: (COND exp, ORD exp, Type a, PrimType' a, Syntax exp (exp a))
    => exp a -> exp a -> exp a
max a b = (a `gte` b) ? a $ b

--------------------------------------------------------------------------------
-- ** Arrays

class ArrIx (exp :: * -> *) where
  arrIx :: Syntax exp a => IArr (Internal a) -> exp Index -> a

instance ArrIx Data where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: PrimType' b => Imp.IArr Index b -> Data b
      ix arr = sugarSymExpPrim (Proxy :: Proxy FeldDomain) (ArrIx arr) i

instance ArrIx HData where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: PrimType' b => Imp.IArr Index b -> HData b
      ix arr = sugarSymExpPrim (Proxy :: Proxy HFeldDomain) (ArrIx arr) i

--------------------------------------------------------------------------------
-- * Programs with computational effects
--------------------------------------------------------------------------------

-- | Monads that support computational effects: mutable data structures and
-- control flow
class Monad m => MonadComp exp m | m -> exp
  where
    -- | Lift a 'Comp' computation
    liftComp :: Comp exp a -> m a
    -- | Conditional statement
    iff :: exp Bool -> m () -> m () -> m ()
    -- | For loop
    for :: (Integral n, PrimType n) => exp n -> (exp n -> m ()) -> m ()
 -- for :: (Integral n, PrimType n) => IxRange (exp n) -> (exp n -> m ()) -> m ()
    -- | While loop
    while :: m (exp Bool) -> m () -> m ()

instance MonadComp exp (Comp exp)
  where
    liftComp        = id
    iff b tru fls   = Comp $ Imp.iff b (unComp tru) (unComp fls)
    for range body  = Comp $ error "Comp:for" --Imp.for (0, 1, Imp.Incl range) (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)

--------------------------------------------------------------------------------
-- ** Syntactic conversion

desugar
  :: ( Syntax exp a, Syntax exp (exp (Internal a))
     , Internal a ~ Internal (exp (Internal a)))
  => a -> exp (Internal a)
desugar = Syntactic.sugar . Syntactic.desugar

sugar
  :: ( Syntax exp a, Syntax exp (exp (Internal a))
     , Internal a ~ Internal (exp (Internal a)))
  => exp (Internal a) -> a
sugar = Syntactic.sugar . Syntactic.desugar

-- | Cast between two values that have the same syntactic representation
resugar :: (Syntax exp a, Syntax exp b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar

--------------------------------------------------------------------------------
-- ** References

-- | Create an initialized reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedRef :: (MonadComp exp m, Syntax exp a)
  => String -- ^ Base name.
  -> a      -- ^ Initial value.
  -> m (Ref (Internal a))
initNamedRef base =
  liftComp . fmap Ref . mapStructA (Comp . Imp.initNamedRef base) . resugar
 
-- | Create an initialized named reference.
initRef :: (MonadComp exp m, Syntax exp a)
  => a -- ^ Initial value.
  -> m (Ref (Internal a))
initRef = initNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: (MonadComp exp m, Type a)
  => String    -- ^ Base name.
  -> m (Ref a)
newNamedRef base =
  liftComp $ fmap Ref $ mapStructA (const $ Comp $ Imp.newNamedRef base) typeRep

-- | Create an uninitialized reference
newRef :: (MonadComp exp m, Type a) => m (Ref a)
newRef = newNamedRef "r"

-- | Get the contents of a reference.
getRef :: forall exp m a. (MonadComp exp m, Syntax exp a, Imp.FreeExp exp, FreeDict exp)
  => Ref (Internal a) -> m a
getRef = liftComp . fmap resugar . mapStructA getty . unRef
  where    
    getty :: forall b. PrimType' b => Imp.Ref b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) Imp.getRef

-- | Set the contents of a reference.
setRef :: forall exp m a. (MonadComp exp m, Syntax exp a, FreeDict exp)
  => Ref (Internal a) -> a -> m ()
setRef r = liftComp . sequence_ . zipListStruct setty (unRef r) . resugar
  where
    setty :: forall b. PrimType' b => Imp.Ref b -> exp b -> Comp exp ()
    setty ref = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.setRef ref)

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: forall exp m a. (MonadComp exp m, Syntax exp a, Imp.FreeExp exp, FreeDict exp)
  => Ref (Internal a) -> m a
unsafeFreezeRef = liftComp . fmap resugar . mapStructA getty . unRef
  where
    getty :: forall b. PrimType' b => Imp.Ref b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) Imp.unsafeFreezeRef

{-
-- | Modify the contents of reference.
modifyRef :: (Syntax a, MonadComp m) => Ref (Internal a) -> (a -> a) -> m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | A version of 'modifyRef' that fixes the value type to @`Data` a@
modifyRefD :: (Type a, MonadComp m) => Ref a -> (Data a -> Data a) -> m ()
modifyRefD r f = setRef r . f =<< unsafeFreezeRef r
-}
--------------------------------------------------------------------------------
-- ** Arrays

-- | Create and initialize a named array.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (MonadComp exp m, PrimType' a)
  => String -- ^ Base name.
  -> [a]    -- ^ Initial contents.
  -> m (Arr a)
initNamedArr base val = liftComp $ fmap (Arr . Single) $ Comp $ Imp.initNamedArr base val

-- | Create and initialize an array.
initArr :: (MonadComp exp m, PrimType' a)
  => [a] -- ^ Initial contents.
  -> m (Arr a)
initArr = initNamedArr "a"

-- | Create an uninitialized named array.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr :: forall exp m a. (MonadComp exp m, Type a)
  => String     -- ^ Base name.
  -> exp Length -- ^ Size.
  -> m (Arr a)
newNamedArr base l =
  liftComp $ fmap Arr $ mapStructA (const $ Comp $ Imp.newNamedArr base l) typeRep

-- | Create an uninitialized array.
newArr :: (MonadComp exp m, Type a)
  => exp Length -- ^ Size.
  -> m (Arr a)
newArr = newNamedArr "a"

-- | Get an element of an array.
getArr :: forall exp m a. (MonadComp exp m, Syntax exp a, Imp.FreeExp exp, FreeDict exp)
  => exp Index -> Arr (Internal a) -> m a
getArr i = liftComp . fmap resugar . mapStructA getty . unArr
  where
    getty :: forall b. PrimType' b => Imp.Arr Length b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.getArr i)

-- | Set an element of an array.
setArr :: forall exp m a. (MonadComp exp m, Syntax exp a, FreeDict exp)
  => exp Index -> a -> Arr (Internal a) -> m ()
setArr i a = liftComp . sequence_ . zipListStruct setty (resugar a) . unArr
  where
    setty :: forall b. PrimType' b => exp b -> Imp.Arr Length b -> Comp exp ()
    setty a arr = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.setArr i a arr)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either array.
copyArr :: forall exp m a. MonadComp exp m
  => Arr a      -- ^ Destination.
  -> Arr a      -- ^ Source.
  -> exp Length -- ^ Number of elements.
  -> m ()
copyArr arr1 arr2 len = liftComp $ sequence_ $ zipListStruct copy (unArr arr1) (unArr arr2)
  where
    copy :: forall b. PrimType' b => Imp.Arr Length b -> Imp.Arr Length b -> Comp exp ()
    copy a1 a2 = Comp $ Imp.copyArr a1 a2 len

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: forall exp m a. (MonadComp exp m, Type a, FreeDict exp)
  => Arr a
  -> exp Length -- ^ Number of elements to copy.
  -> m (IArr a)
freezeArr arr n = liftComp $ fmap IArr $ mapStructA freeze $ unArr arr
  where
    freeze :: forall b. PrimType' b => Imp.Arr Length b -> Comp exp (Imp.IArr Length b)
    freeze a = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.freezeArr a n)

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: forall exp m a. (MonadComp exp m, Type a, FreeDict exp)
  => Arr a
  -> m (IArr a)
unsafeFreezeArr = liftComp . fmap IArr . mapStructA freeze . unArr
  where
    freeze :: forall b. PrimType' b => Imp.Arr Length b -> Comp exp (Imp.IArr Length b)
    freeze = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) Imp.unsafeFreezeArr

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr :: forall exp m a. (MonadComp exp m, Type a, FreeDict exp)
  => IArr a
  -> exp Length -- ^ Number of elements to copy.
  -> m (Arr a)
thawArr arr n = liftComp $ fmap Arr $ mapStructA thaw $ unIArr arr
  where
    thaw :: forall b. PrimType' b => Imp.IArr Length b -> Comp exp (Imp.Arr Length b)
    thaw a = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.thawArr a n)

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr :: forall exp m a. (MonadComp exp m, Type a, FreeDict exp)
  => IArr a
  -> m (Arr a)
unsafeThawArr = liftComp . fmap Arr . mapStructA thaw . unIArr
  where
    thaw :: forall b. PrimType' b => Imp.IArr Length b -> Comp exp (Imp.Arr Length b)
    thaw = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) Imp.unsafeThawArr

-- | Create and initialize an immutable array.
initIArr :: forall exp m a. (MonadComp exp m, PrimType a)
  => [a]
  -> m (IArr a)
initIArr val = liftComp $ fmap (IArr . Single) $ Comp $ Imp.initIArr val

--------------------------------------------------------------------------------
-- ** Control-flow

-- | Conditional statement that returns an expression.
ifE :: (MonadComp exp m, Syntax exp a, Imp.FreeExp exp, FreeDict exp)
    => exp Bool  -- ^ Condition.
    -> m a       -- ^ True branch.
    -> m a       -- ^ False branch.
    -> m a
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res

-- | Break out from a loop
break :: MonadComp exp m => m ()
break = liftComp $ Comp Imp.break

{-
-- | Assertion
assert :: MonadComp m
    => Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> m ()
assert cond msg = liftComp $ Comp $ Imp.assert cond msg
-}
--------------------------------------------------------------------------------
