{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

{-# LANGUAGE TemplateHaskell #-}

{-# options_ghc -fwarn-incomplete-patterns #-}

-- | Primitive software expressions.
module Feldspar.Software.Primitive where

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

import Data.Inhabited

import Feldspar.Representation

--------------------------------------------------------------------------------
-- * Types.
--------------------------------------------------------------------------------

type Length = Word32
type Index  = Word32

-- | Representation of primitive supported types.
data SoftwarePrimTypeRep a
  where
    BoolST          :: SoftwarePrimTypeRep Bool
    Int8ST          :: SoftwarePrimTypeRep Int8
    Int16ST         :: SoftwarePrimTypeRep Int16
    Int32ST         :: SoftwarePrimTypeRep Int32
    Int64ST         :: SoftwarePrimTypeRep Int64
    Word8ST         :: SoftwarePrimTypeRep Word8
    Word16ST        :: SoftwarePrimTypeRep Word16
    Word32ST        :: SoftwarePrimTypeRep Word32
    Word64ST        :: SoftwarePrimTypeRep Word64
    FloatST         :: SoftwarePrimTypeRep Float
    DoubleST        :: SoftwarePrimTypeRep Double
    ComplexFloatST  :: SoftwarePrimTypeRep (Complex Float)
    ComplexDoubleST :: SoftwarePrimTypeRep (Complex Double)

deriving instance Eq       (SoftwarePrimTypeRep a)
deriving instance Show     (SoftwarePrimTypeRep a)
deriving instance Typeable (SoftwarePrimTypeRep a)

--------------------------------------------------------------------------------

viewPrimTypeRep :: SoftwarePrimTypeRep a -> PrimTypeView a
viewPrimTypeRep BoolST          = PrimTypeBool
viewPrimTypeRep Int8ST          = PrimTypeIntWord $ IntType $ Int8Type
viewPrimTypeRep Int16ST         = PrimTypeIntWord $ IntType $ Int16Type
viewPrimTypeRep Int32ST         = PrimTypeIntWord $ IntType $ Int32Type
viewPrimTypeRep Int64ST         = PrimTypeIntWord $ IntType $ Int64Type
viewPrimTypeRep Word8ST         = PrimTypeIntWord $ WordType $ Word8Type
viewPrimTypeRep Word16ST        = PrimTypeIntWord $ WordType $ Word16Type
viewPrimTypeRep Word32ST        = PrimTypeIntWord $ WordType $ Word32Type
viewPrimTypeRep Word64ST        = PrimTypeIntWord $ WordType $ Word64Type
viewPrimTypeRep FloatST         = PrimTypeFloating FloatType
viewPrimTypeRep DoubleST        = PrimTypeFloating DoubleType
viewPrimTypeRep ComplexFloatST  = PrimTypeComplex ComplexFloatType
viewPrimTypeRep ComplexDoubleST = PrimTypeComplex ComplexDoubleType

unviewPrimTypeRep :: PrimTypeView a -> SoftwarePrimTypeRep a
unviewPrimTypeRep PrimTypeBool                            = BoolST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int8Type))    = Int8ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int16Type))   = Int16ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int32Type))   = Int32ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int64Type))   = Int64ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word8Type))  = Word8ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word16Type)) = Word16ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word32Type)) = Word32ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word64Type)) = Word64ST
unviewPrimTypeRep (PrimTypeFloating FloatType)            = FloatST
unviewPrimTypeRep (PrimTypeFloating DoubleType)           = DoubleST
unviewPrimTypeRep (PrimTypeComplex ComplexFloatType)      = ComplexFloatST
unviewPrimTypeRep (PrimTypeComplex ComplexDoubleType)     = ComplexDoubleST

primTypeIntWidth :: SoftwarePrimTypeRep a -> Maybe Int
primTypeIntWidth Int8ST   = Just 8
primTypeIntWidth Int16ST  = Just 16
primTypeIntWidth Int32ST  = Just 32
primTypeIntWidth Int64ST  = Just 64
primTypeIntWidth Word8ST  = Just 8
primTypeIntWidth Word16ST = Just 16
primTypeIntWidth Word32ST = Just 32
primTypeIntWidth Word64ST = Just 64
primTypeIntWidth _        = Nothing

--------------------------------------------------------------------------------
-- ** ... todo: subsection ...

-- | Class of supported software types.
class (Eq a, Show a, Typeable a, Inhabited a) => SoftwarePrimType a
  where
    softwareTypeRep :: SoftwarePrimTypeRep a

instance SoftwarePrimType Bool             where softwareTypeRep = BoolST
instance SoftwarePrimType Int8             where softwareTypeRep = Int8ST
instance SoftwarePrimType Int16            where softwareTypeRep = Int16ST
instance SoftwarePrimType Int32            where softwareTypeRep = Int32ST
instance SoftwarePrimType Int64            where softwareTypeRep = Int64ST
instance SoftwarePrimType Word8            where softwareTypeRep = Word8ST
instance SoftwarePrimType Word16           where softwareTypeRep = Word16ST
instance SoftwarePrimType Word32           where softwareTypeRep = Word32ST
instance SoftwarePrimType Word64           where softwareTypeRep = Word64ST
instance SoftwarePrimType Float            where softwareTypeRep = FloatST
instance SoftwarePrimType Double           where softwareTypeRep = DoubleST
instance SoftwarePrimType (Complex Float)  where softwareTypeRep = ComplexFloatST
instance SoftwarePrimType (Complex Double) where softwareTypeRep = ComplexDoubleST

--------------------------------------------------------------------------------

-- | Convenience function; like 'softwareRep' but with an extra argument to
-- constrain the type parameter. The extra argument is ignored.
softwareTypeOf :: SoftwarePrimType a => a -> SoftwarePrimTypeRep a
softwareTypeOf _ = softwareTypeRep

-- | Reflect a 'SoftwarePrimTypeRep' to a 'SoftwarePrimType' constraint.
witSoftwarePrimType :: SoftwarePrimTypeRep a -> Dict (SoftwarePrimType a)
witSoftwarePrimType BoolST          = Dict
witSoftwarePrimType Int8ST          = Dict
witSoftwarePrimType Int16ST         = Dict
witSoftwarePrimType Int32ST         = Dict
witSoftwarePrimType Int64ST         = Dict
witSoftwarePrimType Word8ST         = Dict
witSoftwarePrimType Word16ST        = Dict
witSoftwarePrimType Word32ST        = Dict
witSoftwarePrimType Word64ST        = Dict
witSoftwarePrimType FloatST         = Dict
witSoftwarePrimType DoubleST        = Dict
witSoftwarePrimType ComplexFloatST  = Dict
witSoftwarePrimType ComplexDoubleST = Dict

--------------------------------------------------------------------------------

instance PrimTypeEq SoftwarePrimTypeRep
  where
    primTypeEq = softwareTypeEq

-- | Check whether two software type representations are equal.
softwareTypeEq :: SoftwarePrimTypeRep a -> SoftwarePrimTypeRep b -> Maybe (Dict (a ~ b))
softwareTypeEq BoolST          BoolST          = Just Dict
softwareTypeEq Int8ST          Int8ST          = Just Dict
softwareTypeEq Int16ST         Int16ST         = Just Dict
softwareTypeEq Int32ST         Int32ST         = Just Dict
softwareTypeEq Int64ST         Int64ST         = Just Dict
softwareTypeEq Word8ST         Word8ST         = Just Dict
softwareTypeEq Word16ST        Word16ST        = Just Dict
softwareTypeEq Word32ST        Word32ST        = Just Dict
softwareTypeEq Word64ST        Word64ST        = Just Dict
softwareTypeEq FloatST         FloatST         = Just Dict
softwareTypeEq DoubleST        DoubleST        = Just Dict
softwareTypeEq ComplexFloatST  ComplexFloatST  = Just Dict
softwareTypeEq ComplexDoubleST ComplexDoubleST = Just Dict
softwareTypeEq _ _ = Nothing

--------------------------------------------------------------------------------
-- * Expressions.
--------------------------------------------------------------------------------

-- | Primitive operations
data SoftwarePrim sig
  where
    FreeVar :: SoftwarePrimType a => String -> SoftwarePrim (Full a)
    Lit     :: (Eq a, Show a) => a -> SoftwarePrim (Full a)

    Add  :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    Sub  :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    Mul  :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    Neg  :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Abs  :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Sign :: (Num a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)

    Quot :: (Integral a, SoftwarePrimType a)   => SoftwarePrim (a :-> a :-> Full a)
    Rem  :: (Integral a, SoftwarePrimType a)   => SoftwarePrim (a :-> a :-> Full a)
    Div  :: (Integral a, SoftwarePrimType a)   => SoftwarePrim (a :-> a :-> Full a)
    Mod  :: (Integral a, SoftwarePrimType a)   => SoftwarePrim (a :-> a :-> Full a)
    FDiv :: (Fractional a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)

    Pi    :: (Floating a, SoftwarePrimType a) => SoftwarePrim (Full a)
    Exp   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Log   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Sqrt  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Pow   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    Sin   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Cos   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Tan   :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Asin  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Acos  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Atan  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Sinh  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Cosh  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Tanh  :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Asinh :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Acosh :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    Atanh :: (Floating a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)

    Complex   :: (Num a, SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (a :-> a :-> Full (Complex a))
    Polar     :: (Floating a, SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (a :-> a :-> Full (Complex a))
    Real      :: (SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (Complex a :-> Full a)
    Imag      :: (SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (Complex a :-> Full a)
    Magnitude :: (RealFloat a, SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (Complex a :-> Full a)
    Phase     :: (RealFloat a, SoftwarePrimType a, SoftwarePrimType (Complex a))
              => SoftwarePrim (Complex a :-> Full a)
    Conjugate :: (Num a, SoftwarePrimType (Complex a))
              => SoftwarePrim (Complex a :-> Full (Complex a))

    I2N   :: (Integral a, Num b, SoftwarePrimType a, SoftwarePrimType b) => SoftwarePrim (a :-> Full b)
    I2B   :: (Integral a, SoftwarePrimType a)                            => SoftwarePrim (a :-> Full Bool)
    B2I   :: (Integral a, SoftwarePrimType a)                            => SoftwarePrim (Bool :-> Full a)
    Round :: (RealFrac a, Num b, SoftwarePrimType a, SoftwarePrimType b) => SoftwarePrim (a :-> Full b)

    Not :: SoftwarePrim (Bool :-> Full Bool)
    And :: SoftwarePrim (Bool :-> Bool :-> Full Bool)
    Or  :: SoftwarePrim (Bool :-> Bool :-> Full Bool)
    Eq  :: (Eq a, SoftwarePrimType a)  => SoftwarePrim (a :-> a :-> Full Bool)
    NEq :: (Eq a, SoftwarePrimType a)  => SoftwarePrim (a :-> a :-> Full Bool)
    Lt  :: (Ord a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full Bool)
    Gt  :: (Ord a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full Bool)
    Le  :: (Ord a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full Bool)
    Ge  :: (Ord a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full Bool)

    BitAnd   :: (Bits a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    BitOr    :: (Bits a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    BitXor   :: (Bits a, SoftwarePrimType a) => SoftwarePrim (a :-> a :-> Full a)
    BitCompl :: (Bits a, SoftwarePrimType a) => SoftwarePrim (a :-> Full a)
    ShiftL   :: (Bits a, SoftwarePrimType a, Integral b, SoftwarePrimType b) => SoftwarePrim (a :-> b :-> Full a)
    ShiftR   :: (Bits a, SoftwarePrimType a, Integral b, SoftwarePrimType b) => SoftwarePrim (a :-> b :-> Full a)

    ArrIx :: SoftwarePrimType a => IArr Index a -> SoftwarePrim (Index :-> Full a)

    Cond :: SoftwarePrim (Bool :-> a :-> a :-> Full a)

--deriving instance Eq       (SoftwarePrim a)
deriving instance Show     (SoftwarePrim a)
deriving instance Typeable (SoftwarePrim a)

-- The `SoftfwarePrimType` constraints on certain symbols require an explanation:
-- They guarantee to the compiler that these symbols don't operate on tuples.
--
-- It would seem more consistent to have a `SoftwarePrimType` constraint on all
-- polymorphic symbols. However, this would prevent using some symbols for
-- non-primitive types in `Feldspar.Software.Representation`. For example, `Lit`
-- and `Cond` are used `Feldspar.Software.Representation`, and there they can
-- also be used for tuple types. The current design was chosen because it
-- "just works".
--------------------------------------------------------------------------------

-- | Software primitive symbols.
type SoftwarePrimConstructs = SoftwarePrim

-- | Software primitive symbols tagged with their type representation.
type SoftwarePrimDomain = SoftwarePrimConstructs :&: SoftwarePrimTypeRep

-- | Software primitive expressions.
newtype SPrim a = SPrim { unSPrim :: ASTF SoftwarePrimDomain a }

--------------------------------------------------------------------------------

-- | Evaluate a closed software expression.
evalSPrim :: SPrim a -> a
evalSPrim = go . unSPrim
  where
    go :: AST SoftwarePrimDomain sig -> Denotation sig
    go (Sym (s :&: _)) = evalSym s
    go (f :$ a)        = go f $ go a

--------------------------------------------------------------------------------

instance Syntactic (SPrim a)
  where
    type Domain   (SPrim a) = SoftwarePrimDomain
    type Internal (SPrim a) = a
    desugar = unSPrim
    sugar   = SPrim

sugarSymSPrim
  :: ( Signature sig
     , fi  ~ SmartFun dom sig
     , sig ~ SmartSig fi
     , dom ~ SmartSym fi
     , dom ~ SoftwarePrimDomain
     , SyntacticN f fi
     , sub :<: SoftwarePrimConstructs
     , SoftwarePrimType (DenResult sig)
     )
  => sub sig -> f
sugarSymSPrim = sugarSymDecor softwareTypeRep

--------------------------------------------------------------------------------
-- ** Interface.

instance (SoftwarePrimType a, Num a) => Num (SPrim a)
  where
    fromInteger = constExp . fromInteger
    (+)         = sugarSymSPrim Add
    (-)         = sugarSymSPrim Sub
    (*)         = sugarSymSPrim Mul
    negate      = sugarSymSPrim Neg
    abs         = sugarSymSPrim Abs
    signum      = sugarSymSPrim Sign

--------------------------------------------------------------------------------
-- imperative-edsl instances.

instance FreeExp SPrim
  where
    type FreePred SPrim = SoftwarePrimType
    constExp = sugarSymSPrim . Lit
    varExp   = sugarSymSPrim . FreeVar

instance EvalExp SPrim
  where
    evalExp = evalSPrim

--------------------------------------------------------------------------------
-- syntactic instances.

deriveSymbol ''SoftwarePrim

instance Render SoftwarePrim
  where
    renderSym (FreeVar v) = v
    renderSym (Lit a)     = show a
    renderSym (ArrIx (IArrComp arr)) = "ArrIx " ++ arr
    renderSym (ArrIx _)              = "ArrIx ..."
    renderSym s = show s

    renderArgs = renderArgsSmart

instance StringTree SoftwarePrim

instance Eval SoftwarePrim
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
instance EvalEnv SoftwarePrim env

instance Equality SoftwarePrim
  where
    equal s1 s2 = show s1 == show s2
      -- NOTE: It is very important not to use `renderSym` here, because it will
      -- render all concrete arrays equal.

      -- This method uses string comparison. It is probably slightly more
      -- efficient to pattern match directly on the constructors. Unfortunately
      -- `deriveEquality ''Primitive` doesn't work, so it gets quite tedious to
      -- write it with pattern matching.

--------------------------------------------------------------------------------
