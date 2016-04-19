module Feldspar.Frontend where



import Prelude (Integral, Floating (..), RealFrac, error)
import qualified Prelude
import Prelude.EDSL

import Control.Monad.Identity
import Data.Int

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Language.Embedded.Imperative (IxRange)
import qualified Language.Embedded.Imperative as Imp

import qualified Data.Inhabited as Inhabited
import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Representation
import Feldspar.Sugar ()



--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

----------------------------------------
-- ** General constructs
----------------------------------------

-- | Explicit sharing
share :: (Syntax a, Syntax b)
    => a         -- ^ Value to share
    -> (a -> b)  -- ^ Body in which to share the value
    -> b
share = shareTag ""

-- | Explicit tagged sharing
shareTag :: (Syntax a, Syntax b)
    => String    -- ^ A tag (that may be empty). May be used by a back end to
                 --   generate a sensible variable name.
    -> a         -- ^ Value to share
    -> (a -> b)  -- ^ Body in which to share the value
    -> b
shareTag tag = sugarSymFeld (Let tag)

-- | For loop
forLoop :: Syntax st => Data Length -> st -> (Data Index -> st -> st) -> st
forLoop = sugarSymFeld ForLoop

-- | Conditional expression
cond :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
cond = sugarSymFeld Cond

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

switch :: (Syntax a, Syntax b, PrimType (Internal a)) =>
    b -> [(Internal a, b)] -> a -> b
switch def [] _ = def
switch def cs s = Prelude.foldr
    (\(c,a) b -> value c == desugar s ? a $ b)
    def
    cs



----------------------------------------
-- ** Literals
----------------------------------------

-- | Literal
value :: Syntax a => Internal a -> a
value = sugarSymFeld . Lit

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

instance (Bounded a, Type a) => Bounded (Data a)
  where
    minBound = value minBound
    maxBound = value maxBound

instance (Num a, PrimType a) => Num (Data a)
  where
    fromInteger = value . fromInteger
    (+)         = sugarSymFeld Add
    (-)         = sugarSymFeld Sub
    (*)         = sugarSymFeld Mul
    negate      = sugarSymFeld Neg
    abs         = sugarSymFeld Abs
    signum      = sugarSymFeld Sign

instance (Fractional a, PrimType a) => Fractional (Data a)
  where
    fromRational = value . fromRational
    (/) = sugarSymFeld FDiv

instance (Floating a, PrimType a) => Floating (Data a)
  where
    pi    = sugarSymFeld Pi
    exp   = sugarSymFeld Exp
    log   = sugarSymFeld Log
    sqrt  = sugarSymFeld Sqrt
    (**)  = sugarSymFeld Pow
    sin   = sugarSymFeld Sin
    cos   = sugarSymFeld Cos
    tan   = sugarSymFeld Tan
    asin  = sugarSymFeld Asin
    acos  = sugarSymFeld Acos
    atan  = sugarSymFeld Atan
    sinh  = sugarSymFeld Sinh
    cosh  = sugarSymFeld Cosh
    tanh  = sugarSymFeld Tanh
    asinh = sugarSymFeld Asinh
    acosh = sugarSymFeld Acosh
    atanh = sugarSymFeld Atanh

π :: (Floating a, PrimType a) => Data a
π = pi

-- | Integer division truncated toward zero
quot :: (Integral a, PrimType a) => Data a -> Data a -> Data a
quot = sugarSymFeld Quot

-- | Integer remainder satisfying
--
-- > (x `quot` y)*y + (x `rem` y) == x
rem :: (Integral a, PrimType a) => Data a -> Data a -> Data a
rem = sugarSymFeld Rem

-- | Simultaneous @quot@ and @rem@
quotRem :: (Integral a, PrimType a) => Data a -> Data a -> (Data a, Data a)
quotRem a b = (q,r)
  where
    q = quot a b
    r = a - b * q

-- | Integral type casting
i2n :: (Integral i, Num n, PrimType i, PrimType n) => Data i -> Data n
i2n = sugarSymFeld I2N

-- | Cast integer to 'Bool'
i2b :: (Integral a, PrimType a) => Data a -> Data Bool
i2b = sugarSymFeld I2B

-- | Cast 'Bool' to integer
b2i :: (Integral a, PrimType a) => Data Bool -> Data a
b2i = sugarSymFeld B2I

-- | Round a floating-point number to an integer
round :: (RealFrac n, Integral i, PrimType i, PrimType n) => Data n -> Data i
round = sugarSymFeld Round

-- | Boolean negation
not :: Data Bool -> Data Bool
not = sugarSymFeld Not

-- | Boolean conjunction
(&&) :: Data Bool -> Data Bool -> Data Bool
(&&) = sugarSymFeld And

infixr 3 &&

-- | Boolean disjunction
(||) :: Data Bool -> Data Bool -> Data Bool
(||) = sugarSymFeld Or

infixr 2 ||


-- | Equality
(==) :: PrimType a => Data a -> Data a -> Data Bool
(==) = sugarSymFeld Eq

-- | Inequality
(/=) :: PrimType a => Data a -> Data a -> Data Bool
a /= b = not (a==b)

-- | Less than
(<) :: PrimType a => Data a -> Data a -> Data Bool
(<) = sugarSymFeld Lt

-- | Greater than
(>) :: PrimType a => Data a -> Data a -> Data Bool
(>) = sugarSymFeld Gt

-- | Less than or equal
(<=) :: PrimType a => Data a -> Data a -> Data Bool
(<=) = sugarSymFeld Le

-- | Greater than or equal
(>=) :: PrimType a => Data a -> Data a -> Data Bool
(>=) = sugarSymFeld Ge

infix 4 ==, /=, <, >, <=, >=

-- | Return the smallest of two values
min :: PrimType a => Data a -> Data a -> Data a
min a b = a<=b ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>

-- | Return the greatest of two values
max :: PrimType a => Data a -> Data a -> Data a
max a b = a>=b ? a $ b



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Index into an array
arrIx :: Syntax a => IArr (Internal a) -> Data Index -> a
arrIx arr i = resugar $ mapStruct ix $ unIArr arr
  where
    ix :: PrimType' b => Imp.IArr Index b -> Data b
    ix arr = sugarSymFeldPrim (ArrIx arr) i



----------------------------------------
-- ** Syntactic conversion
----------------------------------------

desugar :: Syntax a => a -> Data (Internal a)
desugar = Data . Syntactic.desugar

sugar :: Syntax a => Data (Internal a) -> a
sugar = Syntactic.sugar . unData

-- | Cast between two values that have the same syntactic representation
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
    for :: (Integral n, PrimType n) =>
        IxRange (Data n) -> (Data n -> m ()) -> m ()
    -- | While loop
    while :: m (Data Bool) -> m () -> m ()

instance MonadComp Comp
  where
    liftComp        = id
    iff c t f       = Comp $ Imp.iff c (unComp t) (unComp f)
    for range body  = Comp $ Imp.for range (unComp . body)
    while cont body = Comp $ Imp.while (unComp cont) (unComp body)



----------------------------------------
-- ** References
----------------------------------------

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



----------------------------------------
-- ** Arrays
----------------------------------------

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

