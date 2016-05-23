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

class LET exp a where
  shareTag :: Type b => String -> exp a -> (exp a -> exp b) -> exp b

instance (Syntax Data a, Type a) => LET Data a where
  shareTag tag = sugarSymFeld (Let tag)

instance (Syntax HData a, Type a) => LET HData a where
  shareTag tag = sugarSymHFeld (Let tag)

-- | Explicit sharing
share
  :: ( LET exp a
     , Syntax exp a
     , Type b)
  => exp a             -- ^ Value to share
  -> (exp a -> exp b)  -- ^ Body in which to share the value
  -> exp b
share = shareTag ""

--------------------------------------------------------------------------------
-- ** General constructs

class FOR exp st where
  forLoop :: exp Length -> st -> (exp Index -> st -> st) -> st

instance (Syntax Data st) => FOR Data st where
  forLoop = sugarSymFeld ForLoop

instance (Syntax HData st) => FOR HData st where
  forLoop = sugarSymHFeld ForLoop

--------------------------------------------------------------------------------

class COND exp a where
  cond :: exp Bool -> a -> a -> a

instance (Syntax Data a) => COND Data a where
  cond = sugarSymFeld Cond

instance (Syntax HData a) => COND HData a where
  cond = sugarSymHFeld Cond

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: COND exp a
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
min :: (COND exp (exp a), ORD exp a) => exp a -> exp a -> exp a
min a b = (a `lte` b) ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>)

-- | Return the greatest of two values
max :: (COND exp (exp a), ORD exp a) => exp a -> exp a -> exp a
max a b = (a `gte` b) ? a $ b

--------------------------------------------------------------------------------
-- ** Arrays

class ArrIx (exp :: * -> *) a where
  arrIx :: IArr (Internal a) -> exp Index -> a

instance Syntax Data a => ArrIx Data a where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: PrimType' b => Imp.IArr Index b -> Data b
      ix arr = sugarSymExpPrim (Proxy :: Proxy FeldDomain) (ArrIx arr) i

instance Syntax HData a => ArrIx HData a where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: PrimType' b => Imp.IArr Index b -> HData b
      ix arr = sugarSymExpPrim (Proxy :: Proxy HFeldDomain) (ArrIx arr) i

--------------------------------------------------------------------------------
-- * Programs with computational effects
--------------------------------------------------------------------------------

-- | Monads that support computational effects: mutable data structures and
-- control flow
class Monad m => MonadComp exp m
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
    for range body  = Comp $ undefined --Imp.for (0, 1, Imp.Incl range) (unComp . body)
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
initNamedRef
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Type a
     , ExprOf (Domain a) ~ exp
       -- ? ... 
     , Syntactic (Struct PrimType' exp a)
     , Domain    (Struct PrimType' exp a) ~ Domain a
     , Internal  (Struct PrimType' exp a) ~ Internal a)
  => String -- ^ Base name.
  -> a      -- ^ Initial value.
  -> m (Ref a)
initNamedRef base val = liftComp ref
  where
    ref :: Comp exp (Ref a)
    ref = fmap Ref $ mapStructA (Comp . Imp.initNamedRef base) $ resugar val

-- | Create an initialized named reference.
initRef
  :: ( MonadComp exp m
     , Syntax exp a
     , Type a
     , ExprOf (Domain a) ~ exp
       -- ? ... 
     , Syntactic (Struct PrimType' exp a)
     , Domain    (Struct PrimType' exp a) ~ Domain a
     , Internal  (Struct PrimType' exp a) ~ Internal a)
  => a -- ^ Initial value.
  -> m (Ref a)
initRef = initNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef
  :: forall exp m a. (MonadComp exp m, Type a, ExprOf (Domain a) ~ exp)
  => String    -- ^ Base name.
  -> m (Ref a)
newNamedRef base = liftComp ref
  where
    ref :: Comp exp (Ref a)
    ref = fmap Ref $ mapStructA (const $ Comp $ Imp.newNamedRef base) typeRep

-- | Create an uninitialized reference
newRef :: (MonadComp exp m, Type a, ExprOf (Domain a) ~ exp) => m (Ref a)
newRef = newNamedRef "r"

-- | Get the contents of a reference.
getRef
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Syntax exp (Internal a)       
     , Imp.FreeExp exp
     , FreeDict exp
       -- ? ...
     , Syntactic (Struct PrimType' exp (Internal a))
     , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
     , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
  => Ref (Internal a) -> m a
getRef = liftComp . fmap resugar . mapStructA getty . unRef
  where    
    getty :: forall b. (FreeDict exp, Imp.FreeExp exp, PrimType' b) => Imp.Ref b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.getRef)

-- | Set the contents of a reference.
setRef
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Syntax exp (Internal a)
     , FreeDict exp
       -- ? ...
     , Syntactic (Struct PrimType' exp (Internal a))
     , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
     , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
  => Ref (Internal a) -> a -> m ()
setRef r = liftComp . sequence_ . zipListStruct setty (unRef r) . resugar
    -- (\r' a' -> Comp $ Imp.setRef r' a') (unRef r) . resugar
  where
    setty :: forall b. (FreeDict exp, PrimType' b) => Imp.Ref b -> exp b -> Comp exp ()
    setty ref = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.setRef ref)

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Syntax exp (Internal a)
     , Imp.FreeExp exp
     , FreeDict exp
       -- ? ...
     , Syntactic (Struct PrimType' exp (Internal a))
     , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
     , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
  => Ref (Internal a) -> m a
unsafeFreezeRef = liftComp . fmap resugar . mapStructA getty . unRef
  where
    getty :: forall b. (FreeDict exp, Imp.FreeExp exp, PrimType' b) => Imp.Ref b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.unsafeFreezeRef)

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
initNamedArr
  :: forall exp m a.
     ( MonadComp exp m
     , PrimType' a
     , ExprOf (Domain a) ~ exp) -- to avoid ambiguous 'exp'
  => String -- ^ Base name.
  -> [a]    -- ^ Initial contents.
  -> m (Arr a)
initNamedArr base val = liftComp arr
  where
    arr :: Comp exp (Arr a)
    arr = fmap (Arr . Single) $ Comp $ Imp.initNamedArr base val

-- | Create and initialize an array.
initArr
  :: ( MonadComp exp m
     , PrimType' a
     , ExprOf (Domain a) ~ exp)
  => [a] -- ^ Initial contents.
  -> m (Arr a)
initArr = initNamedArr "a"

-- | Create an uninitialized named array.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr
  :: forall exp m a.
     ( MonadComp exp m
     , Type a)
  => String     -- ^ Base name.
  -> exp Length -- ^ Size.
  -> m (Arr a)
newNamedArr base l = liftComp $ fmap Arr $ mapStructA (const arr) typeRep
  where
    arr :: PrimType' b => Comp exp (Imp.Arr Length b)
    arr = Comp $ Imp.newNamedArr base l

-- | Create an uninitialized array.
newArr
  :: ( MonadComp exp m
     , Type a)
  => exp Length -- ^ Size.
  -> m (Arr a)
newArr = newNamedArr "a"

-- | Get an element of an array.
getArr
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Syntax exp (Internal a)
     , Imp.FreeExp exp
     , FreeDict exp
       -- ? ...
     , Syntactic (Struct PrimType' exp (Internal a))
     , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
     , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
  => exp Index -> Arr (Internal a) -> m a
getArr i = liftComp . fmap resugar . mapStructA getty . unArr
  where
    getty :: forall b. (FreeDict exp, Imp.FreeExp exp, PrimType' b) => Imp.Arr Length b -> Comp exp (exp b)
    getty = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.getArr i)

-- | Set an element of an array.
setArr
  :: forall exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Syntax exp (Internal a)
     , FreeDict exp
            -- ? ...
     , Syntactic (Struct PrimType' exp (Internal a))
     , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
     , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
  => exp Index -> a -> Arr (Internal a) -> m ()
setArr i a = liftComp . sequence_ . zipListStruct setty rep . unArr
  where
    rep :: Struct PrimType' exp (Internal a)
    rep = resugar a

    setty :: forall b. (FreeDict exp, PrimType' b) => exp b -> Imp.Arr Length b -> Comp exp ()
    setty a arr = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.setArr i a arr)

-- | Copy the contents of an array to another array. The number of elements to
-- copy must not be greater than the number of allocated elements in either array.
copyArr
  :: forall exp m a.
     (MonadComp exp m)
  => Arr a      -- ^ Destination.
  -> Arr a      -- ^ Source.
  -> exp Length -- ^ Number of elements.
  -> m ()
copyArr arr1 arr2 len = liftComp $ sequence_ $ zipListStruct copy (unArr arr1) (unArr arr2)
  where
    copy :: forall b. (PrimType' b) => Imp.Arr Length b -> Imp.Arr Length b -> Comp exp ()
    copy a1 a2 = Comp $ Imp.copyArr a1 a2 len

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr
  :: forall exp m a.
     ( MonadComp exp m
     , Type a
     , FreeDict exp)
  => Arr a
  -> exp Length -- ^ Number of elements to copy.
  -> m (IArr a)
freezeArr arr n = liftComp $ fmap IArr $ mapStructA freeze $ unArr arr
  where
    freeze :: forall b. (FreeDict exp, PrimType' b) => Imp.Arr Length b -> Comp exp (Imp.IArr Length b)
    freeze a = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.freezeArr a n)

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr
  :: forall exp m a.
     ( MonadComp exp m
     , Type a
     , FreeDict exp
     , ExprOf (Domain a) ~ exp) -- to avoid ambiguous 'exp'
  => Arr a
  -> m (IArr a)
unsafeFreezeArr = liftComp . fmap IArr . mapStructA freeze . unArr
  where
    freeze :: forall b. (FreeDict exp, PrimType' b) => Imp.Arr Length b -> Comp exp (Imp.IArr Length b)
    freeze a = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.unsafeFreezeArr a)

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr
  :: forall exp m a.
     ( MonadComp exp m
     , Type a
     , FreeDict exp)
  => IArr a
  -> exp Length -- ^ Number of elements to copy.
  -> m (Arr a)
thawArr arr n = liftComp $ fmap Arr $ mapStructA thaw $ unIArr arr
  where
    thaw :: forall b. (FreeDict exp, PrimType' b) => Imp.IArr Length b -> Comp exp (Imp.Arr Length b)
    thaw a = Comp $ withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.thawArr a n)

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr
  :: forall exp m a.
     ( MonadComp exp m
     , Type a
     , FreeDict exp
     , ExprOf (Domain a) ~ exp) -- to avoid ambiguous 'exp'
  => IArr a
  -> m (Arr a)
unsafeThawArr = liftComp . fmap Arr . mapStructA thaw . unIArr
  where
    thaw :: forall b. (FreeDict exp, PrimType' b) => Imp.IArr Length b -> Comp exp (Imp.Arr Length b)
    thaw = Comp . withPrim (Proxy::Proxy exp) (Proxy::Proxy b) (Imp.unsafeThawArr)

-- | Create and initialize an immutable array.
initIArr
  :: forall exp m a.
     ( MonadComp exp m
     , PrimType a
     , ExprOf (Domain a) ~ exp)
  => [a]
  -> m (IArr a)
initIArr val = liftComp $ fmap (IArr . Single) $ init
  where
    init :: Comp exp (Imp.IArr Length a)
    init = Comp $ Imp.initIArr val

--------------------------------------------------------------------------------
-- ** Control-flow

-- | Conditional statement that returns an expression.
ifE :: ( MonadComp exp m
       , Syntax exp a
       , Syntax exp (Internal a)
       , Imp.FreeExp exp
       , FreeDict exp
              -- ? ...
       , Syntactic (Struct PrimType' exp (Internal a))
       , Internal  (Struct PrimType' exp (Internal a)) ~ Internal a
       , Domain    (Struct PrimType' exp (Internal a)) ~ Domain a)
    => exp Bool  -- ^ Condition.
    -> m a       -- ^ True branch.
    -> m a       -- ^ False branch.
    -> m a
ifE c t f = do
    res <- newRef
    iff c (t >>= setRef res) (f >>= setRef res)
    unsafeFreezeRef res

{-
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
