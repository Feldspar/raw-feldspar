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

import Data.Inhabited (Inhabited)
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
  shareTag :: (TypeOf exp a, TypeOf exp b)
    => String -> exp a -> (exp a -> exp b) -> exp b

instance LET Data where
  shareTag tag = sugarSymExp (Proxy::Proxy Data) (Let tag)

instance LET HData where
  shareTag tag = sugarSymExp (Proxy::Proxy HData) (Let tag)

-- | Explicit sharing
share
  :: ( LET exp
     , Syntax exp a
     , TypeOf exp a
     , TypeOf exp b)
  => exp a             -- ^ Value to share
  -> (exp a -> exp b)  -- ^ Body in which to share the value
  -> exp b
share = shareTag ""

--------------------------------------------------------------------------------
-- ** General constructs

class FOR exp where
  forLoop :: Syntax exp st => exp Length -> st -> (exp Index -> st -> st) -> st

instance FOR Data where
  forLoop = sugarSymExp (Proxy::Proxy Data) ForLoop

instance FOR HData where
  forLoop = sugarSymExp (Proxy::Proxy HData) HForLoop

--------------------------------------------------------------------------------

class COND exp where
  cond :: Syntax exp a => exp Bool -> a -> a -> a

instance COND Data where
  cond = sugarSymExp (Proxy::Proxy Data) Cond

instance COND HData where
  cond = sugarSymExp (Proxy::Proxy HData) HCond

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

class VAL exp where
  value :: Syntax exp a => Internal a -> a

instance VAL Data where
  value = sugarSymExp (Proxy::Proxy Data) . Lit

instance VAL HData where
  value = sugarSymExp (Proxy::Proxy HData) . HLit

-- | Example value
--
-- 'example' can be used similarly to 'undefined' in normal Haskell, i.e. to
-- create an expression whose value is irrelevant.
--
-- Note that it is generally not possible to use 'undefined' in Feldspar
-- expressions, as this will crash the compiler.
example :: (VAL exp, Syntax exp a, Inhabited (Internal a)) => a
example = value Inhabited.example

--------------------------------------------------------------------------------
-- ** Primitive functions

class NUM (exp :: * -> *) where
  integer :: (PrimTypeOf exp a, Prelude.Num a) => Prelude.Integer -> exp a
  plus    :: (PrimTypeOf exp a, Prelude.Num a) => exp a -> exp a -> exp a
  minus   :: (PrimTypeOf exp a, Prelude.Num a) => exp a -> exp a -> exp a
  times   :: (PrimTypeOf exp a, Prelude.Num a) => exp a -> exp a -> exp a
  negate  :: (PrimTypeOf exp a, Prelude.Num a) => exp a -> exp a

instance NUM Data where
  integer = value . fromInteger
  plus    = sugarSymExp (Proxy::Proxy Data) Add
  minus   = sugarSymExp (Proxy::Proxy Data) Sub
  times   = sugarSymExp (Proxy::Proxy Data) Mul
  negate  = sugarSymExp (Proxy::Proxy Data) Neg

instance NUM HData where
  integer = value . fromInteger
  plus    = sugarSymExp (Proxy::Proxy HData) HAdd
  minus   = sugarSymExp (Proxy::Proxy HData) HSub
  times   = sugarSymExp (Proxy::Proxy HData) HMul
  negate  = sugarSymExp (Proxy::Proxy HData) HNeg

--------------------------------------------------------------------------------

class FRAC (exp :: * -> *) where
  fractional :: (PrimTypeOf exp a, Prelude.Fractional a) => Prelude.Rational -> exp a
  divide     :: (PrimTypeOf exp a, Prelude.Fractional a) => exp a -> exp a -> exp a

instance FRAC Data where
  fractional = value . fromRational
  divide     = sugarSymExp (Proxy::Proxy Data) FDiv

instance FRAC HData where
  fractional = value . fromRational
  divide     = sugarSymExp (Proxy::Proxy HData) HDiv

--------------------------------------------------------------------------------

class FLOAT (exp :: * -> *) where
  pi      :: (PrimTypeOf exp a, Prelude.Floating a) => exp a
  power   :: (PrimTypeOf exp a, Prelude.Floating a) => exp a -> exp a -> exp a
  sinus   :: (PrimTypeOf exp a, Prelude.Floating a) => exp a -> exp a
  cosinus :: (PrimTypeOf exp a, Prelude.Floating a) => exp a -> exp a

instance FLOAT Data where
  pi      = sugarSymExp (Proxy::Proxy Data) Pi
  power   = sugarSymExp (Proxy::Proxy Data) Pow
  sinus   = sugarSymExp (Proxy::Proxy Data) Sin
  cosinus = sugarSymExp (Proxy::Proxy Data) Cos
{-
instance FLOAT HData where
  pi      = sugarSymExp (Proxy::Proxy HData) Pi
  power   = sugarSymExp (Proxy::Proxy HData) Pow
  sinus   = sugarSymExp (Proxy::Proxy HData) Sin
  cosinus = sugarSymExp (Proxy::Proxy HData) Cos
-}
--------------------------------------------------------------------------------

class INTEG (exp :: * -> *) where
  quotient :: (PrimTypeOf exp a, Prelude.Integral a) => exp a -> exp a -> exp a
  reminder :: (PrimTypeOf exp a, Prelude.Integral a) => exp a -> exp a -> exp a
  round    :: (PrimTypeOf exp a, Prelude.Integral a, PrimTypeOf exp n, Prelude.RealFrac n) => exp n -> exp a
  i2n      :: (PrimTypeOf exp a, Prelude.Integral a, PrimTypeOf exp n, Prelude.Num n)      => exp a -> exp n
  i2b      :: (PrimTypeOf exp a, Prelude.Integral a) => exp a -> exp Bool
  b2i      :: (PrimTypeOf exp a, Prelude.Integral a) => exp Bool -> exp a

instance INTEG Data where
  quotient = sugarSymExp (Proxy::Proxy Data) Quot
  reminder = sugarSymExp (Proxy::Proxy Data) Rem
  round    = sugarSymExp (Proxy::Proxy Data) Round
  i2n      = sugarSymExp (Proxy::Proxy Data) I2N
  i2b      = sugarSymExp (Proxy::Proxy Data) I2B
  b2i      = sugarSymExp (Proxy::Proxy Data) B2I

instance INTEG HData where
  quotient = error "!" --sugarSymExp (Proxy::Proxy HData) HQuot
  reminder = sugarSymExp (Proxy::Proxy HData) HRem
  round    = error "!" --sugarSymExp (Proxy::Proxy HData) HRound
  i2n      = sugarSymExp (Proxy::Proxy HData) HI2N
  i2b      = sugarSymExp (Proxy::Proxy HData) HI2B
  b2i      = sugarSymExp (Proxy::Proxy HData) HB2I

-- | Simultaneous @quot@ and @rem@
quotRem
  :: ( NUM exp
     , INTEG exp
     , PrimTypeOf exp a
     , Prelude.Integral a)
  => exp a -> exp a -> (exp a, exp a)
quotRem a b = (q, r)
  where
    q = quotient a b
    r = a `minus` (b `times` q)

--------------------------------------------------------------------------------

class BOOL (exp :: * -> *) where
  true  :: exp Bool
  false :: exp Bool
  not   :: exp Bool -> exp Bool
  and   :: exp Bool -> exp Bool -> exp Bool
  or    :: exp Bool -> exp Bool -> exp Bool

instance BOOL Data where
  true  = value True
  false = value False
  not   = sugarSymExp (Proxy::Proxy Data) Not
  and   = sugarSymExp (Proxy::Proxy Data) And
  or    = sugarSymExp (Proxy::Proxy Data) Or

instance BOOL HData where
  true  = value True
  false = value False
  not   = sugarSymExp (Proxy::Proxy HData) HNot
  and   = sugarSymExp (Proxy::Proxy HData) HAnd
  or    = sugarSymExp (Proxy::Proxy HData) HOr

infixr 3 &&
infixr 2 ||

(&&) :: BOOL exp => exp Bool -> exp Bool -> exp Bool
(&&) = and

(||) :: BOOL exp => exp Bool -> exp Bool -> exp Bool
(||) = or

--------------------------------------------------------------------------------

class EQ (exp :: * -> *) where
  eq  :: (PrimTypeOf exp a, Prelude.Eq a) => exp a -> exp a -> exp Bool
  neq :: (PrimTypeOf exp a, Prelude.Eq a) => exp a -> exp a -> exp Bool

instance EQ Data where
  eq  = sugarSymExp (Proxy::Proxy Data) Eq
  neq = sugarSymExp (Proxy::Proxy Data) NEq

instance EQ HData where
  eq  = sugarSymExp (Proxy::Proxy HData) HEq
  neq = sugarSymExp (Proxy::Proxy HData) HNEq

--------------------------------------------------------------------------------

class ORD (exp :: * -> *) where
  lt  :: (PrimTypeOf exp a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  lte :: (PrimTypeOf exp a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  gt  :: (PrimTypeOf exp a, Prelude.Ord a) => exp a -> exp a -> exp Bool
  gte :: (PrimTypeOf exp a, Prelude.Ord a) => exp a -> exp a -> exp Bool

instance ORD Data where
  lt  = sugarSymExp (Proxy::Proxy Data) Lt
  lte = sugarSymExp (Proxy::Proxy Data) Le
  gt  = sugarSymExp (Proxy::Proxy Data) Gt
  gte = sugarSymExp (Proxy::Proxy Data) Ge

instance ORD HData where
  lt  = sugarSymExp (Proxy::Proxy HData) HLt
  lte = sugarSymExp (Proxy::Proxy HData) HLe
  gt  = sugarSymExp (Proxy::Proxy HData) HGt
  gte = sugarSymExp (Proxy::Proxy HData) HGe

-- | Return the smallest of two values
min :: (COND exp, ORD exp, Type a, PrimTypeOf exp a, Syntax exp (exp a))
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
max :: (COND exp, ORD exp, Type a, PrimTypeOf exp a, Syntax exp (exp a))
    => exp a -> exp a -> exp a
max a b = (a `gte` b) ? a $ b

--------------------------------------------------------------------------------
-- ** Arrays

class ArrIx (exp :: * -> *) where
  arrIx :: Syntax exp a => IArr exp (Internal a) -> exp Index -> a

instance ArrIx Data where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: PrimType' b => Imp.IArr Index b -> Data b
      ix arr = sugarSymExpPrim (Proxy::Proxy Data) (ArrIx arr) i

instance ArrIx HData where
  arrIx arr i = resugar $ mapStruct ix $ unIArr arr
    where
      ix :: HPrimType' b => Imp.IArr Index b -> HData b
      ix arr = sugarSymExpPrim (Proxy::Proxy HData) (HArrIx arr) i

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
initNamedRef :: forall exp m a. (MonadComp exp m, Syntax exp a)
  => String -- ^ Base name.
  -> a      -- ^ Initial value.
  -> m (Ref exp (Internal a))
initNamedRef base a = 
    liftComp $ fmap Ref $ mapStructA (Comp . Imp.initNamedRef base) $ val
  where
    val :: Struct (PredOf exp) exp (Internal a)
    val = resugar a

-- | Create an initialized named reference.
initRef :: (MonadComp exp m, Syntax exp a)
  => a -- ^ Initial value.
  -> m (Ref exp (Internal a))
initRef = initNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: forall exp m a.
     ( MonadComp exp m
     , TypeOf exp a
     , TypedRep exp
     , TypeRepOf exp ~ Struct (PredOf exp) (PrimTypeRepOf exp))
  => String    -- ^ Base name.
  -> m (Ref exp a)
newNamedRef base =
    liftComp $ fmap Ref $ mapStructA (const $ Comp $ Imp.newNamedRef base) val
  where
    val :: Struct (PredOf exp) (PrimTypeRepOf exp) a
    val = represent (Proxy::Proxy exp)

-- | Create an uninitialized reference
newRef
  :: ( MonadComp exp m
     , TypeOf exp a
     , TypedRep exp
     , TypeRepOf exp ~ Struct (PredOf exp) (PrimTypeRepOf exp))
  => m (Ref exp a)
newRef = newNamedRef "r"

-- | Get the contents of a reference.
getRef :: forall proxy proxy2 exp m a.
     ( MonadComp exp m
     , Syntax exp a
     , Imp.FreeExp exp
     , FreeDict exp)
  => Ref exp (Internal a) -> m a
getRef r = liftComp $ fmap resugar $ mapStructA getty $ val
  where
    val :: Struct (PredOf exp) Imp.Ref (Internal a)
    val = unRef r
    
    getty :: forall b. PredOf exp b => Imp.Ref b -> Comp exp (exp b)
    getty = Comp . witPrim (Proxy::Proxy exp) (Proxy::Proxy b) Imp.getRef
{-
    getRef
      :: (Imp.RefCMD Imp.:<: instr, PredOf exp b, Imp.FreePred exp b)
      => Imp.Ref b -> Imp.ProgramT instr (Imp.Param2 exp (PredOf exp)) Identity (exp b)
    getRef = Imp.getRef
-}
{-
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
-}
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
{-
-- | Create and initialize a named array.
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (MonadComp exp m, PrimType' a)
  => String -- ^ Base name.
  -> [a]    -- ^ Initial contents.
  -> m (Arr a)
initNamedArr base val = liftComp $ fmap (Arr . Single) $ Comp $ Imp.initNamedArr base val
-}
{-
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
-}
--------------------------------------------------------------------------------
-- ** Control-flow
{-
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
-}
{-
-- | Assertion
assert :: MonadComp m
    => Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> m ()
assert cond msg = liftComp $ Comp $ Imp.assert cond msg
-}
--------------------------------------------------------------------------------
