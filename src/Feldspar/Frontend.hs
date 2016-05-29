module Feldspar.Frontend where



import Prelude (Integral, Ord, RealFloat, RealFrac)
import qualified Prelude as P
import Prelude.EDSL

import Control.Monad.Identity
import Data.Bits (Bits, FiniteBits)
import qualified Data.Bits as Bits
import Data.Complex (Complex)
import Data.Foldable (Foldable)
import Data.Int
import Data.Traversable (Traversable)

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
switch def cs s = P.foldr
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

-- | Alias for 'pi'
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

-- | Integer division truncated toward negative infinity
div :: (Integral a, PrimType a) => Data a -> Data a -> Data a
div = sugarSymFeld Div

-- | Integer modulus, satisfying
--
-- > (x `div` y)*y + (x `mod` y) == x
mod :: (Integral a, PrimType a) => Data a -> Data a -> Data a
mod = sugarSymFeld Mod

-- | Construct a complex number
complex :: (Num a, PrimType a, PrimType (Complex a))
    => Data a  -- ^ Real part
    -> Data a  -- ^ Imaginary part
    -> Data (Complex a)
complex = sugarSymFeld Complex

-- | Construct a complex number
polar :: (Floating a, PrimType a, PrimType (Complex a))
    => Data a  -- ^ Magnitude
    -> Data a  -- ^ Phase
    -> Data (Complex a)
polar = sugarSymFeld Polar

-- | Extract the real part of a complex number
realPart :: (PrimType a, PrimType (Complex a)) => Data (Complex a) -> Data a
realPart = sugarSymFeld Real

-- | Extract the imaginary part of a complex number
imagPart :: (PrimType a, PrimType (Complex a)) => Data (Complex a) -> Data a
imagPart = sugarSymFeld Imag

-- | Extract the magnitude of a complex number's polar form
magnitude :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    Data (Complex a) -> Data a
magnitude = sugarSymFeld Magnitude

-- | Extract the phase of a complex number's polar form
phase :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    Data (Complex a) -> Data a
phase = sugarSymFeld Phase

-- | Complex conjugate
conjugate :: (RealFloat a, PrimType (Complex a)) =>
    Data (Complex a) -> Data (Complex a)
conjugate = sugarSymFeld Conjugate
  -- `RealFloat` could be replaced by `Num` here, but it seems more consistent
  -- to use `RealFloat` for all functions.

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
round :: (RealFrac a, Num b, PrimType a, PrimType b) => Data a -> Data b
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
(<) :: (Ord a, PrimType a) => Data a -> Data a -> Data Bool
(<) = sugarSymFeld Lt

-- | Greater than
(>) :: (Ord a, PrimType a) => Data a -> Data a -> Data Bool
(>) = sugarSymFeld Gt

-- | Less than or equal
(<=) :: (Ord a, PrimType a) => Data a -> Data a -> Data Bool
(<=) = sugarSymFeld Le

-- | Greater than or equal
(>=) :: (Ord a, PrimType a) => Data a -> Data a -> Data Bool
(>=) = sugarSymFeld Ge

infix 4 ==, /=, <, >, <=, >=

-- | Return the smallest of two values
min :: (Ord a, PrimType a) => Data a -> Data a -> Data a
min a b = a<=b ? a $ b
  -- There's no standard definition of min/max in C:
  -- <http://stackoverflow.com/questions/3437404/min-and-max-in-c>
  --
  -- There is `fmin`/`fminf` for floating-point numbers, but these are
  -- implemented essentially as above (except that they handle `NaN`
  -- specifically:
  -- <https://sourceware.org/git/?p=glibc.git;a=blob;f=math/s_fmin.c;hb=HEAD>

-- | Return the greatest of two values
max :: (Ord a, PrimType a) => Data a -> Data a -> Data a
max a b = a>=b ? a $ b



----------------------------------------
-- ** Bit manipulation
----------------------------------------

-- | Bit-wise \"and\"
(.&.) :: (Bits a, PrimType a) => Data a -> Data a -> Data a
(.&.) = sugarSymFeld BitAnd

-- | Bit-wise \"or\"
(.|.) :: (Bits a, PrimType a) => Data a -> Data a -> Data a
(.|.) = sugarSymFeld BitOr

-- | Bit-wise \"xor\"
xor :: (Bits a, PrimType a) => Data a -> Data a -> Data a
xor = sugarSymFeld BitXor

-- | Bit-wise \"xor\"
(⊕) :: (Bits a, PrimType a) => Data a -> Data a -> Data a
(⊕) = xor

-- | Bit-wise complement
complement :: (Bits a, PrimType a) => Data a -> Data a
complement = sugarSymFeld BitCompl

-- | Left shift
shiftL :: (Bits a, PrimType a)
    => Data a      -- ^ Value to shift
    -> Data Int32  -- ^ Shift amount (negative value gives right shift)
    -> Data a
shiftL = sugarSymFeld ShiftL

-- | Right shift
shiftR :: (Bits a, PrimType a)
    => Data a      -- ^ Value to shift
    -> Data Int32  -- ^ Shift amount (negative value gives left shift)
    -> Data a
shiftR = sugarSymFeld ShiftR

-- | Left shift
(.<<.) :: (Bits a, PrimType a)
    => Data a      -- ^ Value to shift
    -> Data Int32  -- ^ Shift amount (negative value gives right shift)
    -> Data a
(.<<.) = shiftL

-- | Right shift
(.>>.) :: (Bits a, PrimType a)
    => Data a      -- ^ Value to shift
    -> Data Int32  -- ^ Shift amount (negative value gives left shift)
    -> Data a
(.>>.) = shiftR

infixl 8 `shiftL`, `shiftR`, .<<., .>>.
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

bitSize :: forall a . FiniteBits a => Data a -> Length
bitSize _ = P.fromIntegral $ Bits.finiteBitSize (a :: a)
  where
    a = P.error "finiteBitSize evaluates its argument"

-- | Set all bits to one
allOnes :: (Bits a, Num a, PrimType a) => Data a
allOnes = complement 0

-- | Set the @n@ lowest bits to one
oneBits :: (Bits a, Num a, PrimType a) => Data Int32 -> Data a
oneBits n = complement (allOnes .<<. n)

-- | Extract the @k@ lowest bits
lsbs :: (Bits a, Num a, PrimType a) => Data Int32 -> Data a -> Data a
lsbs k i = i .&. oneBits k

-- | Integer logarithm in base 2
ilog2 :: (FiniteBits a, Integral a, PrimType a) => Data a -> Data a
ilog2 a = snd $ P.foldr (\ffi vr -> share vr (step ffi)) (a,0) ffis
  where
    step (ff,i) (v,r) =
        share (b2i (v > fromInteger ff) .<<. value i) $ \shift ->
          (v .>>. i2n shift, r .|. shift)

    -- [(0x1, 0), (0x3, 1), (0xF, 2), (0xFF, 3), (0xFFFF, 4), ...]
    ffis
        = (`P.zip` [0..])
        $ P.takeWhile (P.<= (2 P.^ (bitSize a `P.div` 2) - 1 :: Integer))
        $ P.map ((subtract 1) . (2 P.^) . (2 P.^))
        $ [(0::Integer)..]
  -- Based on this algorithm:
  -- <http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog>



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Index into an array
arrIx :: Syntax a => IArr (Internal a) -> Data Index -> a
arrIx arr i = resugar $ mapStruct ix $ unIArr arr
  where
    ix :: PrimType' b => Imp.IArr Index b -> Data b
    ix arr' = sugarSymFeldPrim (ArrIx arr') (i + iarrOffset arr)

class Indexed a
  where
    type IndexedElem a

    -- | Indexing operator. If @a@ is 'Finite', it is assumed that
    -- @i < `length` a@ in any expression @a `!` i@.
    (!) :: a -> Data Index -> IndexedElem a

infixl 9 !

-- | Linear structures with a length. If the type is also 'Indexed', the length
-- is the successor of the maximal allowed index.
class Finite a
  where
    -- | The length of a finite structure
    length :: a -> Data Length

instance Type a => Indexed (IArr a)
  where
    type IndexedElem (IArr a) = Data a
    (!) = arrIx

-- | Make a dimension-less value 1-dimensional by pairing it with a length
--
-- A typical use of 'Dim1' is @`Dim1` (`IArr` a)@.
data Dim1 a = Dim1
    { dimLength  :: Data Length
    , dim1_inner :: a
    }
  deriving (Functor, Foldable, Traversable)

instance Indexed a => Indexed (Dim1 a)
  where
    type IndexedElem (Dim1 a) = IndexedElem a
    Dim1 _ a ! i = a!i

instance Indexed a => Finite (Dim1 a)
  where
    length = dimLength

-- | Make a dimension-less value 2-dimensional by pairing it with a pair of
-- lengths
--
-- A typical use of 'Dim2' is @`Dim2` (`IArr` a)@.
data Dim2 a = Dim2
    { dimRows    :: Data Length
    , dimCols    :: Data Length
    , dim2_inner :: a
    }
  deriving (Functor, Foldable, Traversable)

-- | Linear row-major indexing
instance Indexed a => Indexed (Dim2 a)
  where
    type IndexedElem (Dim2 a) = IndexedElem a
    Dim2 _ _ a ! i = a!i

-- | Length is `#rows * #columns`
instance Indexed a => Finite (Dim2 a)
  where
    length (Dim2 r c _) = r*c



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



----------------------------------------
-- ** Unsafe operations
----------------------------------------

-- | Turn a 'Comp' computation into a pure value. For this to be safe, the
-- computation should be free of side effects and independent of its
-- environment.
unsafePerform :: Syntax a => Comp a -> a
unsafePerform = sugarSymFeld . UnsafePerform . fmap desugar

-- | Attach a 'Comp' action to an expression. Evaluation of the expression will
-- cause the action to run. For this to be safe, the action should be free of
-- side effects and independent of its environment.
unsafePerformWith :: Syntax a => Comp () -> a -> a
unsafePerformWith = sugarSymFeld . UnsafePerformWith



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
newNamedArr base l = liftComp $ fmap (Arr 0) $
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
    liftComp . fmap (Arr 0 . Single) . Comp . Imp.initNamedArr base

-- | Get an element of an array
getArr :: (Syntax a, MonadComp m) => Data Index -> Arr (Internal a) -> m a
getArr i arr
    = liftComp
    $ fmap resugar
    $ mapStructA (Comp . Imp.getArr (i + arrOffset arr))
    $ unArr arr

-- | Set an element of an array
setArr :: forall m a . (Syntax a, MonadComp m) =>
    Data Index -> a -> Arr (Internal a) -> m ()
setArr i a arr
    = liftComp
    $ sequence_
    $ zipListStruct
        (\a' arr' -> Comp $ Imp.setArr (i + arrOffset arr) a' arr') rep
    $ unArr arr
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
    zipListStruct
      (\a1 a2 ->
          Comp $ Imp.copyArr (a1, arrOffset arr1) (a2, arrOffset arr2) len
      )
      (unArr arr1)
      (unArr arr2)

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (Type a, MonadComp m)
    => Arr a
    -> Data Length  -- ^ Length of new array
    -> m (IArr a)
freezeArr arr n = liftComp $ do
    arr2 <- newArr n
    copyArr arr2 arr n
    unsafeFreezeArr arr2
  -- This is better than calling `freezeArr` from imperative-edsl, since that
  -- one copies without offset.

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: (Type a, MonadComp m) => Arr a -> m (IArr a)
unsafeFreezeArr arr
    = liftComp
    $ fmap (IArr (arrOffset arr))
    $ mapStructA (Comp . Imp.unsafeFreezeArr)
    $ unArr arr

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr :: (Type a, MonadComp m)
    => IArr a
    -> Data Length  -- ^ Number of elements to copy
    -> m (Arr a)
thawArr arr n = liftComp $ do
    arr2 <- unsafeThawArr arr
    arr3 <- newArr n
    copyArr arr3 arr2 n
    return arr3

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr :: (Type a, MonadComp m) => IArr a -> m (Arr a)
unsafeThawArr arr
    = liftComp
    $ fmap (Arr (iarrOffset arr))
    $ mapStructA (Comp . Imp.unsafeThawArr)
    $ unIArr arr

-- | Create and initialize an immutable array
initIArr :: (PrimType a, MonadComp m) => [a] -> m (IArr a)
initIArr = liftComp . fmap (IArr 0 . Single) . Comp . Imp.initIArr



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

