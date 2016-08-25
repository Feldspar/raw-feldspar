module Feldspar.Frontend where



import Prelude (Integral, Ord, RealFloat, RealFrac)
import qualified Prelude as P
import Prelude.EDSL

import Control.Monad.Identity
import Data.Bits (Bits, FiniteBits)
import qualified Data.Bits as Bits
import Data.Complex (Complex)
import Data.Int
import Data.List (genericLength)

import Language.Syntactic (Internal)
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import qualified Control.Monad.Operational.Higher as Oper
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

-- | Force evaluation of a value and share the result. Note that due to common
-- sub-expression elimination, this function is rarely needed in practice.
share :: (Syntax a, Syntax b)
    => a         -- ^ Value to share
    -> (a -> b)  -- ^ Body in which to share the value
    -> b
share = shareTag ""
  -- Explicit sharing can be useful e.g. when the value to share contains a
  -- function or when the code motion algorithm for some reason doesn't work
  -- find opportunities for sharing.

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
-- @
-- cond1 `?` a $
-- cond2 `?` b $
-- cond3 `?` c $
--         default
-- @
(?) :: Syntax a
    => Data Bool  -- ^ Condition
    -> a          -- ^ True branch
    -> a          -- ^ False branch
    -> a
(?) = cond

infixl 1 ?

-- | Multi-way conditional expression
--
-- The first association @(a,b)@ in the list of cases for which @a@ is equal to
-- the scrutinee is selected, and the associated @b@ is returned as the result.
-- If no case matches, the default value is returned.
switch :: (Syntax a, Syntax b, PrimType (Internal a))
    => b        -- ^ Default result
    -> [(a,b)]  -- ^ Cases (match, result)
    -> a        -- ^ Scrutinee
    -> b        -- ^ Result
switch def [] _ = def
switch def cs s = P.foldr
    (\(c,a) b -> desugar c == desugar s ? a $ b)
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

-- | Integer division assuming `unsafeBalancedDiv x y * y == x` (i.e. no
-- remainder)
--
-- The advantage of using 'unsafeBalancedDiv' over 'quot' or 'div' is that the
-- above assumption can be used for simplifying the expression.
unsafeBalancedDiv :: (Integral a, PrimType a) => Data a -> Data a -> Data a
unsafeBalancedDiv a b = guardValLabel
    InternalAssertion
    (rem a b == 0)
    "unsafeBalancedDiv: division not balanced"
    (sugarSymFeld DivBalanced a b)
  -- Note: We can't check that `result * b == a`, because `result * b` gets
  -- simplified to `a`.

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

-- | Integer logarithm in base 2. Returns \(\lfloor log_2(x) \rfloor\).
-- Assumes \(x>0\).
ilog2 :: (FiniteBits a, Integral a, PrimType a) => Data a -> Data a
ilog2 a = guardValLabel InternalAssertion (a >= 1) "ilog2: argument < 1" $
    snd $ P.foldr (\ffi vr -> share vr (step ffi)) (a,0) ffis
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
arrIx :: Syntax a => IArr a -> Data Index -> a
arrIx arr i = resugar $ mapStruct ix $ unIArr arr
  where
    ix :: forall b . PrimType' b => Imp.IArr Index b -> Data b
    ix arr' = sugarSymFeldPrim
      (GuardVal InternalAssertion "arrIx: index out of bounds")
      (i < length arr)
      (sugarSymFeldPrim (ArrIx arr') (i + iarrOffset arr) :: Data b)

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

instance Finite (Arr a)  where length = arrLength
instance Finite (IArr a) where length = iarrLength

-- | Linear structures that can be sliced
class Slicable a
  where
    -- | Take a slice of a structure
    slice
        :: Data Index   -- ^ Start index
        -> Data Length  -- ^ Slice length
        -> a            -- ^ Structure to slice
        -> a

instance Syntax a => Indexed (IArr a)
  where
    type IndexedElem (IArr a) = a
    (!) = arrIx

instance Slicable (Arr a)
  where
    slice from len (Arr o l arr) = Arr o' l' arr
      where
        o' = guardValLabel InternalAssertion (from<=l) "invalid Arr slice" (o+from)
        l' = guardValLabel InternalAssertion (from+len<=l) "invalid Arr slice" len

instance Slicable (IArr a)
  where
    slice from len (IArr o l arr) = IArr o' l' arr
      where
        o' = guardValLabel InternalAssertion (from<=l) "invalid IArr slice" (o+from)
        l' = guardValLabel InternalAssertion (from+len<=l) "invalid IArr slice" len



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
-- ** Assertions
----------------------------------------

-- | Guard a value by an assertion (with implicit label @`UserAssertion` ""@)
guardVal :: Syntax a
    => Data Bool  -- ^ Condition that is expected to be true
    -> String     -- ^ Error message
    -> a          -- ^ Value to attach the assertion to
    -> a
guardVal = guardValLabel $ UserAssertion ""

-- | Like 'guardVal' but with an explicit assertion label
guardValLabel :: Syntax a
    => AssertionLabel  -- ^ Assertion label
    -> Data Bool       -- ^ Condition that is expected to be true
    -> String          -- ^ Error message
    -> a               -- ^ Value to attach the assertion to
    -> a
guardValLabel c cond msg = sugarSymFeld (GuardVal c msg) cond



----------------------------------------
-- ** Unsafe operations
----------------------------------------

-- | Turn a 'Comp' computation into a pure value. For this to be safe, the
-- computation should be free of side effects and independent of its
-- environment.
unsafePerform :: Syntax a => Comp a -> a
unsafePerform = sugarSymFeld . UnsafePerform . fmap desugar



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
newRef :: (Syntax a, MonadComp m) => m (Ref a)
newRef = newNamedRef "r"

-- | Create an uninitialized named reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedRef :: (Syntax a, MonadComp m)
    => String  -- ^ Base name
    -> m (Ref a)
newNamedRef base = liftComp $ fmap Ref $
    mapStructA (const $ Comp $ Imp.newNamedRef base) typeRep

-- | Create an initialized named reference
initRef :: (Syntax a, MonadComp m) => a -> m (Ref a)
initRef = initNamedRef "r"

-- | Create an initialized reference
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedRef :: (Syntax a, MonadComp m)
    => String  -- ^ Base name
    -> a       -- ^ Initial value
    -> m (Ref a)
initNamedRef base =
    liftComp . fmap Ref . mapStructA (Comp . Imp.initNamedRef base) . resugar

-- | Get the contents of a reference.
getRef :: (Syntax a, MonadComp m) => Ref a -> m a
getRef = liftComp . fmap resugar . mapStructA (Comp . Imp.getRef) . unRef

-- | Set the contents of a reference.
setRef :: (Syntax a, MonadComp m) => Ref a -> a -> m ()
setRef r
    = liftComp
    . sequence_
    . zipListStruct (\r' a' -> Comp $ Imp.setRef r' a') (unRef r)
    . resugar

-- | Modify the contents of reference.
modifyRef :: (Syntax a, MonadComp m) => Ref a -> (a -> a) -> m ()
modifyRef r f = setRef r . f =<< unsafeFreezeRef r

-- | Freeze the contents of reference (only safe if the reference is not updated
--   as long as the resulting value is alive).
unsafeFreezeRef :: (Syntax a, MonadComp m) => Ref a -> m a
unsafeFreezeRef
    = liftComp
    . fmap resugar
    . mapStructA (Comp . Imp.unsafeFreezeRef)
    . unRef



----------------------------------------
-- ** Arrays
----------------------------------------

-- | Create an uninitialized array
newArr :: (Type (Internal a), MonadComp m) => Data Length -> m (Arr a)
newArr = newNamedArr "a"

-- | Create an uninitialized named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
newNamedArr :: (Type (Internal a), MonadComp m)
    => String  -- ^ Base name
    -> Data Length
    -> m (Arr a)
newNamedArr base l = liftComp $ fmap (Arr 0 l) $
    mapStructA (const (Comp $ Imp.newNamedArr base l)) typeRep

-- | Create and initialize an array
initArr :: (PrimType (Internal a), MonadComp m)
    => [Internal a]  -- ^ Initial contents
    -> m (Arr a)
initArr = initNamedArr "a"

-- It would seem

-- | Create and initialize a named array
--
-- The provided base name may be appended with a unique identifier to avoid name
-- collisions.
initNamedArr :: (PrimType (Internal a), MonadComp m)
    => String        -- ^ Base name
    -> [Internal a]  -- ^ Initial contents
    -> m (Arr a)
initNamedArr base as =
    liftComp $ fmap (Arr 0 len . Single) $ Comp $ Imp.initNamedArr base as
  where
    len = value $ genericLength as

-- | Get an element of an array
getArr :: (Syntax a, MonadComp m) => Data Index -> Arr a -> m a
getArr i arr = do
    assertLabel
      InternalAssertion
      (i < length arr)
      "getArr: index out of bounds"
    liftComp
      $ fmap resugar
      $ mapStructA (Comp . Imp.getArr (i + arrOffset arr))
      $ unArr arr

-- | Set an element of an array
setArr :: forall m a . (Syntax a, MonadComp m) =>
    Data Index -> a -> Arr a -> m ()
setArr i a arr = do
    assertLabel
      InternalAssertion
      (i < length arr)
      "setArr: index out of bounds"
    liftComp
      $ sequence_
      $ zipListStruct
          (\a' arr' -> Comp $ Imp.setArr (i + arrOffset arr) a' arr') rep
      $ unArr arr
  where
    rep = resugar a :: Struct PrimType' Data (Internal a)

-- | Copy the contents of an array to another array. The length of the
-- destination array must not be less than that of the source array.
--
-- In order to copy only a part of an array, use 'slice' before calling
-- 'copyArr'.
copyArr :: MonadComp m
    => Arr a  -- ^ Destination
    -> Arr a  -- ^ Source
    -> m ()
copyArr arr1 arr2 = do
    assertLabel
      InternalAssertion
      (length arr1 >= length arr2)
      "copyArr: destination too small"
    liftComp $ sequence_ $
      zipListStruct
        (\a1 a2 ->
            Comp $ Imp.copyArr
              (a1, arrOffset arr1)
              (a2, arrOffset arr2)
              (length arr2)
        )
        (unArr arr1)
        (unArr arr2)

-- | Freeze a mutable array to an immutable one. This involves copying the array
-- to a newly allocated one.
freezeArr :: (Type (Internal a), MonadComp m) => Arr a -> m (IArr a)
freezeArr arr = liftComp $ do
    arr2 <- newArr (length arr)
    copyArr arr2 arr
    unsafeFreezeArr arr2
  -- This is better than calling `freezeArr` from imperative-edsl, since that
  -- one copies without offset.

-- | Freeze a mutable array to an immutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeFreezeArr :: MonadComp m => Arr a -> m (IArr a)
unsafeFreezeArr arr
    = liftComp
    $ fmap (IArr (arrOffset arr) (length arr))
    $ mapStructA (Comp . Imp.unsafeFreezeArr)
    $ unArr arr

-- | Thaw an immutable array to a mutable one. This involves copying the array
-- to a newly allocated one.
thawArr :: (Type (Internal a), MonadComp m) => IArr a -> m (Arr a)
thawArr arr = liftComp $ do
    arr2 <- unsafeThawArr arr
    arr3 <- newArr (length arr)
    copyArr arr3 arr2
    return arr3

-- | Thaw an immutable array to a mutable one without making a copy. This is
-- generally only safe if the the mutable array is not updated as long as the
-- immutable array is alive.
unsafeThawArr :: MonadComp m => IArr a -> m (Arr a)
unsafeThawArr arr
    = liftComp
    $ fmap (Arr (iarrOffset arr) (length arr))
    $ mapStructA (Comp . Imp.unsafeThawArr)
    $ unIArr arr

-- | Create and initialize an immutable array
initIArr :: (PrimType (Internal a), MonadComp m) =>
    [Internal a] -> m (IArr a)
initIArr = initArr >=> unsafeFreezeArr



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

-- | Assertion (with implicit label @`UserAssertion` ""@)
assert :: MonadComp m
    => Data Bool  -- ^ Expression that should be true
    -> String     -- ^ Message in case of failure
    -> m ()
assert = assertLabel $ UserAssertion ""

-- | Like 'assert' but tagged with an explicit assertion label
assertLabel :: MonadComp m
    => AssertionLabel  -- ^ Assertion label
    -> Data Bool       -- ^ Expression that should be true
    -> String          -- ^ Message in case of failure
    -> m ()
assertLabel c cond msg =
    liftComp $ Comp $ Oper.singleInj $ Assert c cond msg



----------------------------------------
-- ** Misc.
----------------------------------------

-- | Force evaluation of a value and share the result (monadic version of
-- 'share')
shareM :: (Syntax a, MonadComp m) => a -> m a
shareM = initRef >=> unsafeFreezeRef
  -- This function is more commonly needed than `share`, since code motion
  -- doesn't work across monadic binds.

