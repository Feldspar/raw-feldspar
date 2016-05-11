{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Primitive Feldspar expressions

module Feldspar.Primitive.Representation where



import Data.Array
import Data.Bits
import Data.Complex
import Data.Int
import Data.Typeable
import Data.Word

import Data.Constraint (Dict (..))

import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD (IArr (..))

import Language.Syntactic
import Language.Syntactic.TH
import Language.Syntactic.Functional



--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

type Length = Word32
type Index  = Word32

-- | Representation of primitive supported types
data PrimTypeRep a
  where
    BoolT          :: PrimTypeRep Bool
    Int8T          :: PrimTypeRep Int8
    Int16T         :: PrimTypeRep Int16
    Int32T         :: PrimTypeRep Int32
    Int64T         :: PrimTypeRep Int64
    Word8T         :: PrimTypeRep Word8
    Word16T        :: PrimTypeRep Word16
    Word32T        :: PrimTypeRep Word32
    Word64T        :: PrimTypeRep Word64
    FloatT         :: PrimTypeRep Float
    DoubleT        :: PrimTypeRep Double
    ComplexFloatT  :: PrimTypeRep (Complex Float)
    ComplexDoubleT :: PrimTypeRep (Complex Double)

data IntTypeRep a
  where
    Int8Type  :: IntTypeRep Int8
    Int16Type :: IntTypeRep Int16
    Int32Type :: IntTypeRep Int32
    Int64Type :: IntTypeRep Int64

data WordTypeRep a
  where
    Word8Type  :: WordTypeRep Word8
    Word16Type :: WordTypeRep Word16
    Word32Type :: WordTypeRep Word32
    Word64Type :: WordTypeRep Word64

data IntWordTypeRep a
  where
    IntType  :: IntTypeRep a -> IntWordTypeRep a
    WordType :: WordTypeRep a -> IntWordTypeRep a

data FloatingTypeRep a
  where
    FloatType  :: FloatingTypeRep Float
    DoubleType :: FloatingTypeRep Double

data ComplexTypeRep a
  where
    ComplexFloatType  :: ComplexTypeRep (Complex Float)
    ComplexDoubleType :: ComplexTypeRep (Complex Double)

-- | A different view of 'PrimTypeRep' that allows matching on similar types
data PrimTypeView a
  where
    PrimTypeBool     :: PrimTypeView Bool
    PrimTypeIntWord  :: IntWordTypeRep a -> PrimTypeView a
    PrimTypeFloating :: FloatingTypeRep a -> PrimTypeView a
    PrimTypeComplex  :: ComplexTypeRep a -> PrimTypeView a

deriving instance Show (PrimTypeRep a)
deriving instance Show (IntTypeRep a)
deriving instance Show (WordTypeRep a)
deriving instance Show (IntWordTypeRep a)
deriving instance Show (FloatingTypeRep a)
deriving instance Show (ComplexTypeRep a)
deriving instance Show (PrimTypeView a)

viewPrimTypeRep :: PrimTypeRep a -> PrimTypeView a
viewPrimTypeRep BoolT          = PrimTypeBool
viewPrimTypeRep Int8T          = PrimTypeIntWord $ IntType $ Int8Type
viewPrimTypeRep Int16T         = PrimTypeIntWord $ IntType $ Int16Type
viewPrimTypeRep Int32T         = PrimTypeIntWord $ IntType $ Int32Type
viewPrimTypeRep Int64T         = PrimTypeIntWord $ IntType $ Int64Type
viewPrimTypeRep Word8T         = PrimTypeIntWord $ WordType $ Word8Type
viewPrimTypeRep Word16T        = PrimTypeIntWord $ WordType $ Word16Type
viewPrimTypeRep Word32T        = PrimTypeIntWord $ WordType $ Word32Type
viewPrimTypeRep Word64T        = PrimTypeIntWord $ WordType $ Word64Type
viewPrimTypeRep FloatT         = PrimTypeFloating FloatType
viewPrimTypeRep DoubleT        = PrimTypeFloating DoubleType
viewPrimTypeRep ComplexFloatT  = PrimTypeComplex ComplexFloatType
viewPrimTypeRep ComplexDoubleT = PrimTypeComplex ComplexDoubleType

unviewPrimTypeRep :: PrimTypeView a -> PrimTypeRep a
unviewPrimTypeRep PrimTypeBool                            = BoolT
unviewPrimTypeRep (PrimTypeIntWord (IntType Int8Type))    = Int8T
unviewPrimTypeRep (PrimTypeIntWord (IntType Int16Type))   = Int16T
unviewPrimTypeRep (PrimTypeIntWord (IntType Int32Type))   = Int32T
unviewPrimTypeRep (PrimTypeIntWord (IntType Int64Type))   = Int64T
unviewPrimTypeRep (PrimTypeIntWord (WordType Word8Type))  = Word8T
unviewPrimTypeRep (PrimTypeIntWord (WordType Word16Type)) = Word16T
unviewPrimTypeRep (PrimTypeIntWord (WordType Word32Type)) = Word32T
unviewPrimTypeRep (PrimTypeIntWord (WordType Word64Type)) = Word64T
unviewPrimTypeRep (PrimTypeFloating FloatType)            = FloatT
unviewPrimTypeRep (PrimTypeFloating DoubleType)           = DoubleT
unviewPrimTypeRep (PrimTypeComplex ComplexFloatType)      = ComplexFloatT
unviewPrimTypeRep (PrimTypeComplex ComplexDoubleType)     = ComplexDoubleT

primTypeIntWidth :: PrimTypeRep a -> Maybe Int
primTypeIntWidth Int8T   = Just 8
primTypeIntWidth Int16T  = Just 16
primTypeIntWidth Int32T  = Just 32
primTypeIntWidth Int64T  = Just 64
primTypeIntWidth Word8T  = Just 8
primTypeIntWidth Word16T = Just 16
primTypeIntWidth Word32T = Just 32
primTypeIntWidth Word64T = Just 64
primTypeIntWidth _       = Nothing

-- | Primitive supported types
class (Eq a, Show a, Typeable a) => PrimType' a
  where
    -- | Reify a primitive type
    primTypeRep :: PrimTypeRep a

instance PrimType' Bool             where primTypeRep = BoolT
instance PrimType' Int8             where primTypeRep = Int8T
instance PrimType' Int16            where primTypeRep = Int16T
instance PrimType' Int32            where primTypeRep = Int32T
instance PrimType' Int64            where primTypeRep = Int64T
instance PrimType' Word8            where primTypeRep = Word8T
instance PrimType' Word16           where primTypeRep = Word16T
instance PrimType' Word32           where primTypeRep = Word32T
instance PrimType' Word64           where primTypeRep = Word64T
instance PrimType' Float            where primTypeRep = FloatT
instance PrimType' Double           where primTypeRep = DoubleT
instance PrimType' (Complex Float)  where primTypeRep = ComplexFloatT
instance PrimType' (Complex Double) where primTypeRep = ComplexDoubleT

-- | Convenience function; like 'primTypeRep' but with an extra argument to
-- constrain the type parameter. The extra argument is ignored.
primTypeOf :: PrimType' a => a -> PrimTypeRep a
primTypeOf _ = primTypeRep

-- | Check whether two type representations are equal
primTypeEq :: PrimTypeRep a -> PrimTypeRep b -> Maybe (Dict (a ~ b))
primTypeEq BoolT          BoolT          = Just Dict
primTypeEq Int8T          Int8T          = Just Dict
primTypeEq Int16T         Int16T         = Just Dict
primTypeEq Int32T         Int32T         = Just Dict
primTypeEq Int64T         Int64T         = Just Dict
primTypeEq Word8T         Word8T         = Just Dict
primTypeEq Word16T        Word16T        = Just Dict
primTypeEq Word32T        Word32T        = Just Dict
primTypeEq Word64T        Word64T        = Just Dict
primTypeEq FloatT         FloatT         = Just Dict
primTypeEq DoubleT        DoubleT        = Just Dict
primTypeEq ComplexFloatT  ComplexFloatT  = Just Dict
primTypeEq ComplexDoubleT ComplexDoubleT = Just Dict
primTypeEq _ _ = Nothing

-- | Reflect a 'PrimTypeRep' to a 'PrimType'' constraint
witPrimType :: PrimTypeRep a -> Dict (PrimType' a)
witPrimType BoolT          = Dict
witPrimType Int8T          = Dict
witPrimType Int16T         = Dict
witPrimType Int32T         = Dict
witPrimType Int64T         = Dict
witPrimType Word8T         = Dict
witPrimType Word16T        = Dict
witPrimType Word32T        = Dict
witPrimType Word64T        = Dict
witPrimType FloatT         = Dict
witPrimType DoubleT        = Dict
witPrimType ComplexFloatT  = Dict
witPrimType ComplexDoubleT = Dict



--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- | Primitive operations
data Primitive sig
  where
    FreeVar :: PrimType' a => String -> Primitive (Full a)
    Lit     :: (Eq a, Show a) => a -> Primitive (Full a)

    Add  :: (Num a, PrimType' a) => Primitive (a :-> a :-> Full a)
    Sub  :: (Num a, PrimType' a) => Primitive (a :-> a :-> Full a)
    Mul  :: (Num a, PrimType' a) => Primitive (a :-> a :-> Full a)
    Neg  :: (Num a, PrimType' a) => Primitive (a :-> Full a)
    Abs  :: (Num a, PrimType' a) => Primitive (a :-> Full a)
    Sign :: (Num a, PrimType' a) => Primitive (a :-> Full a)

    Quot :: (Integral a, PrimType' a)   => Primitive (a :-> a :-> Full a)
    Rem  :: (Integral a, PrimType' a)   => Primitive (a :-> a :-> Full a)
    Div  :: (Integral a, PrimType' a)   => Primitive (a :-> a :-> Full a)
    Mod  :: (Integral a, PrimType' a)   => Primitive (a :-> a :-> Full a)
    FDiv :: (Fractional a, PrimType' a) => Primitive (a :-> a :-> Full a)

    Pi    :: (Floating a, PrimType' a) => Primitive (Full a)
    Exp   :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Log   :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Sqrt  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Pow   :: (Floating a, PrimType' a) => Primitive (a :-> a :-> Full a)
    Sin   :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Cos   :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Tan   :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Asin  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Acos  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Atan  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Sinh  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Cosh  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Tanh  :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Asinh :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Acosh :: (Floating a, PrimType' a) => Primitive (a :-> Full a)
    Atanh :: (Floating a, PrimType' a) => Primitive (a :-> Full a)

    Complex   :: (Num a, PrimType' a, PrimType' (Complex a))       => Primitive (a :-> a :-> Full (Complex a))
    Polar     :: (Floating a, PrimType' a, PrimType' (Complex a))  => Primitive (a :-> a :-> Full (Complex a))
    Real      :: (PrimType' a, PrimType' (Complex a))              => Primitive (Complex a :-> Full a)
    Imag      :: (PrimType' a, PrimType' (Complex a))              => Primitive (Complex a :-> Full a)
    Magnitude :: (RealFloat a, PrimType' a, PrimType' (Complex a)) => Primitive (Complex a :-> Full a)
    Phase     :: (RealFloat a, PrimType' a, PrimType' (Complex a)) => Primitive (Complex a :-> Full a)
    Conjugate :: (Num a, PrimType' (Complex a))                    => Primitive (Complex a :-> Full (Complex a))

    I2N   :: (Integral a, Num b, PrimType' a, PrimType' b)      => Primitive (a :-> Full b)
    I2B   :: (Integral a, PrimType' a)                          => Primitive (a :-> Full Bool)
    B2I   :: (Integral a, PrimType' a)                          => Primitive (Bool :-> Full a)
    Round :: (RealFrac a, Num b, PrimType' a, PrimType' b) => Primitive (a :-> Full b)

    Not :: Primitive (Bool :-> Full Bool)
    And :: Primitive (Bool :-> Bool :-> Full Bool)
    Or  :: Primitive (Bool :-> Bool :-> Full Bool)
    Eq  :: (Eq a, PrimType' a)  => Primitive (a :-> a :-> Full Bool)
    NEq :: (Eq a, PrimType' a)  => Primitive (a :-> a :-> Full Bool)
    Lt  :: (Ord a, PrimType' a) => Primitive (a :-> a :-> Full Bool)
    Gt  :: (Ord a, PrimType' a) => Primitive (a :-> a :-> Full Bool)
    Le  :: (Ord a, PrimType' a) => Primitive (a :-> a :-> Full Bool)
    Ge  :: (Ord a, PrimType' a) => Primitive (a :-> a :-> Full Bool)

    BitAnd   :: (Bits a, PrimType' a) => Primitive (a :-> a :-> Full a)
    BitOr    :: (Bits a, PrimType' a) => Primitive (a :-> a :-> Full a)
    BitXor   :: (Bits a, PrimType' a) => Primitive (a :-> a :-> Full a)
    BitCompl :: (Bits a, PrimType' a) => Primitive (a :-> Full a)
    ShiftL   :: (Bits a, PrimType' a, Integral b, PrimType' b) => Primitive (a :-> b :-> Full a)
    ShiftR   :: (Bits a, PrimType' a, Integral b, PrimType' b) => Primitive (a :-> b :-> Full a)

    ArrIx :: PrimType' a => IArr Index a -> Primitive (Index :-> Full a)

    Cond :: Primitive (Bool :-> a :-> a :-> Full a)

deriving instance Show (Primitive a)

-- The `PrimType'` constraints on certain symbols require an explanation: The
-- constraints are actually not needed for anything in the modules in
-- `Feldspar.Primitive.*`, but they are needed by `Feldspar.Run.Compile`. They
-- guarantee to the compiler that these symbols don't operate on tuples.
--
-- It would seem more consistent to have a `PrimType'` constraint on all
-- polymorphic symbols. However, this would prevent using some symbols for
-- non-primitive types in `Feldspar.Representation`. For example, `Lit` and
-- `Cond` are used `Feldspar.Representation`, and there they can also be used
-- for tuple types. The current design was chosen because it "just works".

deriveSymbol ''Primitive

instance Render Primitive
  where
    renderSym (FreeVar v) = v
    renderSym (Lit a)     = show a
    renderSym (ArrIx (IArrComp arr)) = "ArrIx " ++ arr
    renderSym (ArrIx _)              = "ArrIx ..."
    renderSym s = show s

    renderArgs = renderArgsSmart

instance StringTree Primitive

instance Eval Primitive
  where
    evalSym (FreeVar v) = error $ "evaluating free variable " ++ show v
    evalSym (Lit a)     = a
    evalSym Add         = (+)
    evalSym Sub         = (-)
    evalSym Mul         = (*)
    evalSym Neg         = negate
    evalSym Abs         = abs
    evalSym Sign        = signum
    evalSym Quot        = quot
    evalSym Rem         = rem
    evalSym Div         = div
    evalSym Mod         = mod
    evalSym FDiv        = (/)
    evalSym Pi          = pi
    evalSym Exp         = exp
    evalSym Log         = log
    evalSym Sqrt        = sqrt
    evalSym Pow         = (**)
    evalSym Sin         = sin
    evalSym Cos         = cos
    evalSym Tan         = tan
    evalSym Asin        = asin
    evalSym Acos        = acos
    evalSym Atan        = atan
    evalSym Sinh        = sinh
    evalSym Cosh        = cosh
    evalSym Tanh        = tanh
    evalSym Asinh       = asinh
    evalSym Acosh       = acosh
    evalSym Atanh       = atanh
    evalSym Complex     = (:+)
    evalSym Polar       = mkPolar
    evalSym Real        = realPart
    evalSym Imag        = imagPart
    evalSym Magnitude   = magnitude
    evalSym Phase       = phase
    evalSym Conjugate   = conjugate
    evalSym I2N         = fromInteger . toInteger
    evalSym I2B         = (/=0)
    evalSym B2I         = \a -> if a then 1 else 0
    evalSym Round       = fromInteger . round
    evalSym Not         = not
    evalSym And         = (&&)
    evalSym Or          = (||)
    evalSym Eq          = (==)
    evalSym NEq         = (/=)
    evalSym Lt          = (<)
    evalSym Gt          = (>)
    evalSym Le          = (<=)
    evalSym Ge          = (>=)
    evalSym BitAnd      = (.&.)
    evalSym BitOr       = (.|.)
    evalSym BitXor      = xor
    evalSym BitCompl    = complement
    evalSym ShiftL      = \a -> shiftL a . fromIntegral
    evalSym ShiftR      = \a -> shiftR a . fromIntegral
    evalSym Cond        = \c t f -> if c then t else f
    evalSym (ArrIx (IArrRun arr)) = \i ->
        if i<l || i>h
          then error $ "ArrIx: index "
                    ++ show (toInteger i)
                    ++ " out of bounds "
                    ++ show (toInteger l, toInteger h)
          else arr!i
      where
        (l,h) = bounds arr
    evalSym (ArrIx (IArrComp arr)) = error $ "evaluating symbolic array " ++ arr

-- | Assumes no occurrences of 'FreeVar' and concrete representation of arrays
instance EvalEnv Primitive env

instance Equality Primitive
  where
    equal s1 s2 = show s1 == show s2
      -- NOTE: It is very important not to use `renderSym` here, because it will
      -- render all concrete arrays equal.

      -- This method uses string comparison. It is probably slightly more
      -- efficient to pattern match directly on the constructors. Unfortunately
      -- `deriveEquality ''Primitive` doesn't work, so it gets quite tedious to
      -- write it with pattern matching.

type PrimDomain = Primitive :&: PrimTypeRep

-- | Primitive expressions
newtype Prim a = Prim { unPrim :: ASTF PrimDomain a }

instance Syntactic (Prim a)
  where
    type Domain (Prim a)   = PrimDomain
    type Internal (Prim a) = a
    desugar = unPrim
    sugar   = Prim

-- | Evaluate a closed expression
evalPrim :: Prim a -> a
evalPrim = go . unPrim
  where
    go :: AST PrimDomain sig -> Denotation sig
    go (Sym (s :&: _)) = evalSym s
    go (f :$ a) = go f $ go a

sugarSymPrim
    :: ( Signature sig
       , fi  ~ SmartFun dom sig
       , sig ~ SmartSig fi
       , dom ~ SmartSym fi
       , dom ~ PrimDomain
       , SyntacticN f fi
       , sub :<: Primitive
       , PrimType' (DenResult sig)
       )
    => sub sig -> f
sugarSymPrim = sugarSymDecor primTypeRep

instance FreeExp Prim
  where
    type FreePred Prim = PrimType'
    constExp = sugarSymPrim . Lit
    varExp   = sugarSymPrim . FreeVar

instance EvalExp Prim
  where
    evalExp = evalPrim



--------------------------------------------------------------------------------
-- * Interface
--------------------------------------------------------------------------------

instance (Num a, PrimType' a) => Num (Prim a)
  where
    fromInteger = constExp . fromInteger
    (+)         = sugarSymPrim Add
    (-)         = sugarSymPrim Sub
    (*)         = sugarSymPrim Mul
    negate      = sugarSymPrim Neg
    abs         = sugarSymPrim Abs
    signum      = sugarSymPrim Sign

