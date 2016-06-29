{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Primitive Feldspar expressions
module Feldspar.Primitive.Representation where

import Data.Array
import Data.Int
import Data.Typeable
import Data.Word
import Data.Bits (Bits)
import qualified Data.Bits as Bits

import Data.Constraint (Dict (..))

import Language.Embedded.Imperative.CMD (IArr (..))
import qualified Language.Embedded.Expression as Imp

import qualified Language.Embedded.Hardware as Hard

import Language.Syntactic
import Language.Syntactic.TH
import Language.Syntactic.Functional

import GHC.TypeLits (KnownNat, sameNat)
import Data.Type.Equality ((:~:))

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

type Length = Word32
type Index  = Word32

-- | ...
class (Eq a, Ord a, Show a, Typeable a) => PrimType' (prim :: * -> *) (a :: *)
  where
    primTypeRep :: proxy prim -> PrimTypeRep a

-- | Representation of primitive supported types
data PrimTypeRep a
  where
    BoolT   :: PrimTypeRep Bool
    -- ^ fixed-size signed.
    Int8T   :: PrimTypeRep Int8
    Int16T  :: PrimTypeRep Int16
    Int32T  :: PrimTypeRep Int32
    Int64T  :: PrimTypeRep Int64
    -- ^ fixed-size unsigned.
    Word8T  :: PrimTypeRep Word8
    Word16T :: PrimTypeRep Word16
    Word32T :: PrimTypeRep Word32
    Word64T :: PrimTypeRep Word64
    -- ^ floating-point.
    FloatT  :: PrimTypeRep Float
    DoubleT :: PrimTypeRep Double
    -- ^ variable length signed.
    IntT    :: PrimTypeRep Integer
    -- ^ variable length unsigned.
    BitsT   :: (Typeable n, KnownNat n) => PrimTypeRep (Hard.Bits n)
    UBitsT  :: PrimTypeRep (Hard.UBits)

--------------------------------------------------------------------------------

-- | Convenience function; like 'primTypeRep' but with an extra argument to
-- constrain the type parameter. The extra argument is ignored.
primTypeOf :: PrimType' prim a => proxy prim -> a -> PrimTypeRep a
primTypeOf p _ = primTypeRep p

-- | Check whether two type representations are equal
primTypeEq :: forall a b. PrimTypeRep a -> PrimTypeRep b -> Maybe (Dict (a ~ b))
primTypeEq BoolT   BoolT   = Just Dict
primTypeEq Int8T   Int8T   = Just Dict
primTypeEq Int16T  Int16T  = Just Dict
primTypeEq Int32T  Int32T  = Just Dict
primTypeEq Int64T  Int64T  = Just Dict
primTypeEq Word8T  Word8T  = Just Dict
primTypeEq Word16T Word16T = Just Dict
primTypeEq Word32T Word32T = Just Dict
primTypeEq Word64T Word64T = Just Dict
primTypeEq FloatT  FloatT  = Just Dict
primTypeEq DoubleT DoubleT = Just Dict
primTypeEq IntT    IntT    = Just Dict
primTypeEq BitsT   BitsT   =
  case sameNat (peelProxy (Proxy::Proxy a)) (peelProxy (Proxy::Proxy b)) of
    Just Refl -> Just Dict
    Nothing   -> Nothing
  where
    peelProxy :: KnownNat n => Proxy (Hard.Bits n) -> Proxy n
    peelProxy _ = Proxy
primTypeEq UBitsT  UBitsT  = Just Dict
primTypeEq _       _       = Nothing

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- | Primitive operations
data Primitive sig
  where
    FreeVar :: String -> Primitive (Full a)
    Lit     :: (Eq a, Ord a, Show a) => a -> Primitive (Full a)
    Pi      :: (Floating a, PType' a) => Primitive (Full a)

    Add :: (Num a, PType' a) => Primitive (a :-> a :-> Full a)
    Sub :: (Num a, PType' a) => Primitive (a :-> a :-> Full a)
    Mul :: (Num a, PType' a) => Primitive (a :-> a :-> Full a)
    Neg :: (Num a, PType' a) => Primitive (a :-> Full a)

    Quot :: (Integral a, PType' a)   => Primitive (a :-> a :-> Full a)
    Rem  :: (Integral a, PType' a)   => Primitive (a :-> a :-> Full a)
    FDiv :: (Fractional a, PType' a) => Primitive (a :-> a :-> Full a)

    Sin :: (Floating a, PType' a) => Primitive (a :-> Full a)
    Cos :: (Floating a, PType' a) => Primitive (a :-> Full a)
    Pow :: (Floating a, PType' a) => Primitive (a :-> a :-> Full a)

    I2N   :: (Integral a, Num b, PType' a, PType' b)      => Primitive (a :-> Full b)
    I2B   :: (Integral a, PType' a)                       => Primitive (a :-> Full Bool)
    B2I   :: (Integral a, PType' a)                       => Primitive (Bool :-> Full a)
    Round :: (RealFrac a, Integral b, PType' a, PType' b) => Primitive (a :-> Full b)

    Not :: Primitive (Bool :-> Full Bool)
    And :: Primitive (Bool :-> Bool :-> Full Bool)
    Or  :: Primitive (Bool :-> Bool :-> Full Bool)
    Eq  :: (Eq a, PType' a)  => Primitive (a :-> a :-> Full Bool)
    NEq :: (Eq a, PType' a)  => Primitive (a :-> a :-> Full Bool)
    Lt  :: (Ord a, PType' a) => Primitive (a :-> a :-> Full Bool)
    Gt  :: (Ord a, PType' a) => Primitive (a :-> a :-> Full Bool)
    Le  :: (Ord a, PType' a) => Primitive (a :-> a :-> Full Bool)
    Ge  :: (Ord a, PType' a) => Primitive (a :-> a :-> Full Bool)

    ArrIx :: IArr Index a -> Primitive (Index :-> Full a)

    Cond :: Primitive (Bool :-> a :-> a :-> Full a)

--------------------------------------------------------------------------------
-- ** ...

-- | ...
type PrimDomain = Primitive :&: PrimTypeRep

-- | Primitive expressions
newtype Prim a = Prim { unPrim :: ASTF PrimDomain a }

-- | ...
type PType' = PrimType' Prim

instance Syntactic (Prim a)
  where
    type Domain   (Prim a) = PrimDomain
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

-- | ...
sugarSymPrim
    :: forall sig dom fi sub f prim.
       ( Signature sig
       , fi  ~ SmartFun dom sig
       , sig ~ SmartSig fi
       , dom ~ SmartSym fi
       , dom ~ PrimDomain
       , SyntacticN f fi
       , sub :<: Primitive
       , PType' (DenResult sig)
       )
    => sub sig -> f
sugarSymPrim = sugarSymDecor $ primTypeRep (Proxy::Proxy Prim)

--------------------------------------------------------------------------------
-- ** Representable types for 'Prim'.

instance PrimType' Prim Bool   where primTypeRep _ = BoolT
instance PrimType' Prim Int8   where primTypeRep _ = Int8T
instance PrimType' Prim Int16  where primTypeRep _ = Int16T
instance PrimType' Prim Int32  where primTypeRep _ = Int32T
instance PrimType' Prim Int64  where primTypeRep _ = Int64T
instance PrimType' Prim Word8  where primTypeRep _ = Word8T
instance PrimType' Prim Word16 where primTypeRep _ = Word16T
instance PrimType' Prim Word32 where primTypeRep _ = Word32T
instance PrimType' Prim Word64 where primTypeRep _ = Word64T
instance PrimType' Prim Float  where primTypeRep _ = FloatT
instance PrimType' Prim Double where primTypeRep _ = DoubleT

-- | Reflect a 'PrimTypeRep' to a 'PrimType'' constraint over 'Prim'.
witPrimType :: PrimTypeRep a -> Dict (PrimType' Prim a)
witPrimType BoolT   = Dict
witPrimType Int8T   = Dict
witPrimType Int16T  = Dict
witPrimType Int32T  = Dict
witPrimType Int64T  = Dict
witPrimType Word8T  = Dict
witPrimType Word16T = Dict
witPrimType Word32T = Dict
witPrimType Word64T = Dict
witPrimType FloatT  = Dict
witPrimType DoubleT = Dict
--witPrimType IntT    = error "witPrimType: add maybe?"
--witPrimType BitsT   = error "witPrimType: add maybe?"
--witPrimType UBitsT  = error "witPrimType: add maybe?"

--------------------------------------------------------------------------------

instance Imp.FreeExp Prim
  where
    type FreePred Prim = PType'
    constExp = sugarSymPrim . Lit
    varExp   = sugarSymPrim . FreeVar

instance Hard.FreeExp Prim
  where
    type PredicateExp Prim = PType'
    litE = sugarSymPrim . Lit
    varE = sugarSymPrim . FreeVar

instance Imp.EvalExp Prim
  where
    evalExp = evalPrim

instance Hard.EvaluateExp Prim
  where
    evalE = evalPrim

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Primitive hardware operations.
data HPrimitive sig
  where
    HFreeVar :: String -> HPrimitive (Full a)
    HLit     :: (Eq a, Ord a, Show a) => a -> HPrimitive (Full a)

    HNot :: HPrimitive (Bool :-> Full Bool)
    HAnd :: HPrimitive (Bool :-> Bool :-> Full Bool)
    HOr  :: HPrimitive (Bool :-> Bool :-> Full Bool)
    
    HBAnd  :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HBOr   :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HBXor  :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HBXnor :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HBNand :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HBNor  :: (Bits a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    
    HEq  :: (Eq a, HPType' a)  => HPrimitive (a :-> a :-> Full Bool)
    HNEq :: (Eq a, HPType' a)  => HPrimitive (a :-> a :-> Full Bool)
    HLt  :: (Ord a, HPType' a) => HPrimitive (a :-> a :-> Full Bool)
    HGt  :: (Ord a, HPType' a) => HPrimitive (a :-> a :-> Full Bool)
    HLe  :: (Ord a, HPType' a) => HPrimitive (a :-> a :-> Full Bool)
    HGe  :: (Ord a, HPType' a) => HPrimitive (a :-> a :-> Full Bool)

    HAdd :: (Num a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HSub :: (Num a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HMul :: (Num a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HNeg :: (Num a, HPType' a) => HPrimitive (a :-> Full a)

    HRem  :: (Integral a, HPType' a)   => HPrimitive (a :-> a :-> Full a)
    HDiv  :: (Fractional a, HPType' a) => HPrimitive (a :-> a :-> Full a)
    HPow  :: (Floating a, HPType' a)   => HPrimitive (a :-> a :-> Full a) 

    HI2N   :: (Integral a, Num b, HPType' a, HPType' b) => HPrimitive (a :-> Full b)
    HI2B   :: (Integral a, HPType' a) => HPrimitive (a :-> Full Bool)
    HB2I   :: (Integral a, HPType' a) => HPrimitive (Bool :-> Full a)

    HArrIx :: IArr Index a -> HPrimitive (Index :-> Full a)

    HCond  :: HPrimitive (Bool :-> a :-> a :-> Full a)

--------------------------------------------------------------------------------
-- ** ...

-- | ...
type HPrimDomain = HPrimitive :&: PrimTypeRep

-- | ...
newtype HPrim a = HPrim { unHPrim :: ASTF HPrimDomain a }

-- | ...
type HPType' = PrimType' HPrim

instance Syntactic (HPrim a)
  where
    type Domain   (HPrim a) = HPrimDomain
    type Internal (HPrim a) = a
    desugar = unHPrim
    sugar = HPrim

-- | ...
evalHPrim :: HPrim a -> a
evalHPrim = go . unHPrim
  where
    go :: AST HPrimDomain sig -> Denotation sig
    go (Sym (s :&: _)) = evalSym s
    go (f :$ a) = go f $ go a

-- | ...
sugarSymHPrim
    :: ( Signature sig
       , fi  ~ SmartFun dom sig
       , sig ~ SmartSig fi
       , dom ~ SmartSym fi
       , dom ~ HPrimDomain
       , SyntacticN f fi
       , sub :<: HPrimitive
       , HPType' (DenResult sig)
       )
    => sub sig -> f
sugarSymHPrim = sugarSymDecor $ primTypeRep (Proxy::Proxy HPrim)

--------------------------------------------------------------------------------
-- | Representable types for 'HPrim'.

instance PrimType' HPrim Bool       where primTypeRep _ = BoolT
instance PrimType' HPrim Int8       where primTypeRep _ = Int8T
instance PrimType' HPrim Int16      where primTypeRep _ = Int16T
instance PrimType' HPrim Int32      where primTypeRep _ = Int32T
instance PrimType' HPrim Int64      where primTypeRep _ = Int64T
instance PrimType' HPrim Word8      where primTypeRep _ = Word8T
instance PrimType' HPrim Word16     where primTypeRep _ = Word16T
instance PrimType' HPrim Word32     where primTypeRep _ = Word32T
instance PrimType' HPrim Word64     where primTypeRep _ = Word64T
instance PrimType' HPrim Float      where primTypeRep _ = FloatT
instance PrimType' HPrim Double     where primTypeRep _ = DoubleT
instance PrimType' HPrim Integer    where primTypeRep _ = IntT
instance PrimType' HPrim Hard.UBits where primTypeRep _ = UBitsT
instance (Typeable n, KnownNat n) =>
      PrimType' HPrim (Hard.Bits n) where primTypeRep _ = BitsT

-- | Reflect a 'PrimTypeRep' to a 'PrimType'' constraint over 'HPrim'.
witHPrimType :: PrimTypeRep a -> Dict (PrimType' HPrim a)
witHPrimType BoolT   = Dict
witHPrimType Int8T   = Dict
witHPrimType Int16T  = Dict
witHPrimType Int32T  = Dict
witHPrimType Int64T  = Dict
witHPrimType Word8T  = Dict
witHPrimType Word16T = Dict
witHPrimType Word32T = Dict
witHPrimType Word64T = Dict
witHPrimType FloatT  = Dict
witHPrimType DoubleT = Dict
witHPrimType IntT    = Dict
witHPrimType BitsT   = Dict
witHPrimType UBitsT  = Dict

--------------------------------------------------------------------------------

instance Imp.FreeExp HPrim
  where
    type FreePred HPrim = HPType'
    constExp = sugarSymHPrim . HLit
    varExp   = sugarSymHPrim . HFreeVar

instance Hard.FreeExp HPrim
  where
    type PredicateExp HPrim = HPType'
    litE = sugarSymHPrim . HLit
    varE = sugarSymHPrim . HFreeVar

instance Imp.EvalExp HPrim
  where
    evalExp = evalHPrim

instance Hard.EvaluateExp HPrim
  where
    evalE = evalHPrim

--------------------------------------------------------------------------------
-- * Interface.
--------------------------------------------------------------------------------

instance (Num a, PrimType' Prim a) => Num (Prim a)
  where
    fromInteger = Imp.constExp . fromInteger
    (+)         = sugarSymPrim Add
    (-)         = sugarSymPrim Sub
    (*)         = sugarSymPrim Mul
    negate      = sugarSymPrim Neg
    abs         = error "abs not implemented for Prim"
    signum      = error "signum not implemented for Prim"

instance (Num a, PrimType' HPrim a) => Num (HPrim a)
  where
    fromInteger = Imp.constExp . fromInteger
    (+)         = sugarSymHPrim HAdd
    (-)         = sugarSymHPrim HSub
    (*)         = sugarSymHPrim HMul
    negate      = sugarSymHPrim HNeg
    abs         = error "abs not implemented for HPrim"
    signum      = error "signum not implemented for HPrim"

--------------------------------------------------------------------------------
-- Instances.
--------------------------------------------------------------------------------

deriveSymbol ''Primitive

instance Render Primitive
  where
    renderSym (FreeVar v) = v
    renderSym (Lit a)     = show a
    renderSym Pi          = "Pi"
    renderSym Add         = "(+)"
    renderSym Sub         = "(-)"
    renderSym Mul         = "(*)"
    renderSym Neg         = "Neg"
    renderSym Quot        = "Quot"
    renderSym Rem         = "Rem"
    renderSym FDiv        = "FDiv"
    renderSym Sin         = "Sin"
    renderSym Cos         = "Cos"
    renderSym Pow         = "Pow"
    renderSym I2N         = "I2N"
    renderSym I2B         = "I2B"
    renderSym B2I         = "B2I"
    renderSym Round       = "Round"
    renderSym Not         = "Not"
    renderSym And         = "And"
    renderSym Or          = "Or"
    renderSym Eq          = "(==)"
    renderSym NEq         = "(/=)"
    renderSym Lt          = "(<)"
    renderSym Gt          = "(>)"
    renderSym Le          = "(<=)"
    renderSym Ge          = "(>=)"
    renderSym Cond        = "Cond"
    renderSym (ArrIx (IArrComp arr)) = "ArrIx " ++ arr
    renderSym (ArrIx _)              = "ArrIx ..."

    renderArgs = renderArgsSmart

instance StringTree Primitive

instance Eval Primitive
  where
    evalSym (FreeVar v) = error $ "evaluating free variable " ++ show v
    evalSym (Lit a)     = a
    evalSym Pi          = pi
    evalSym Add         = (+)
    evalSym Sub         = (-)
    evalSym Mul         = (*)
    evalSym Neg         = negate
    evalSym Quot        = quot
    evalSym Rem         = rem
    evalSym FDiv        = (/)
    evalSym Sin         = sin
    evalSym Cos         = cos
    evalSym Pow         = (**)
    evalSym I2N         = fromInteger . toInteger
    evalSym I2B         = (/=0)
    evalSym B2I         = \a -> if a then 1 else 0
    evalSym Round       = round
    evalSym Not         = not
    evalSym And         = (&&)
    evalSym Or          = (||)
    evalSym Eq          = (==)
    evalSym NEq         = (/=)
    evalSym Lt          = (<)
    evalSym Gt          = (>)
    evalSym Le          = (<=)
    evalSym Ge          = (>=)
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

-- | Returns false for arrays that don't have a symbolic representation
instance Equality Primitive
  where
    equal (FreeVar v) (FreeVar w) = v==w
    equal (Lit a)     (Lit b)     = show a == show b
    equal Pi          Pi          = True
    equal Add         Add         = True
    equal Sub         Sub         = True
    equal Mul         Mul         = True
    equal Neg         Neg         = True
    equal Quot        Quot        = True
    equal Rem         Rem         = True
    equal FDiv        FDiv        = True
    equal Sin         Sin         = True
    equal Cos         Cos         = True
    equal Pow         Pow         = True
    equal I2N         I2N         = True
    equal I2B         I2B         = True
    equal B2I         B2I         = True
    equal Round       Round       = True
    equal Not         Not         = True
    equal And         And         = True
    equal Or          Or          = True
    equal Eq          Eq          = True
    equal NEq         NEq         = True
    equal Lt          Lt          = True
    equal Gt          Gt          = True
    equal Le          Le          = True
    equal Ge          Ge          = True
    equal Cond        Cond        = True
    equal (ArrIx (IArrComp arr1)) (ArrIx (IArrComp arr2)) = arr1==arr2
    equal (ArrIx _) (ArrIx _) = False
    equal _ _ = False

--------------------------------------------------------------------------------

deriveSymbol ''HPrimitive

instance Render HPrimitive
  where
    renderSym (HFreeVar v) = v
    renderSym (HLit a)     = show a
    renderSym HAdd         = "(+)"
    renderSym HSub         = "(-)"
    renderSym HMul         = "(*)"
    renderSym HNeg         = "Neg"
    renderSym HRem         = "Rem"
    renderSym HDiv         = "Div"
    renderSym HPow         = "Pow"
    renderSym HI2N         = "I2N"
    renderSym HI2B         = "I2B"
    renderSym HB2I         = "B2I"
    renderSym HNot         = "Not"
    renderSym HAnd         = "And"
    renderSym HOr          = "Or"
    renderSym HEq          = "(==)"
    renderSym HNEq         = "(/=)"
    renderSym HLt          = "(<)"
    renderSym HGt          = "(>)"
    renderSym HLe          = "(<=)"
    renderSym HGe          = "(>=)"
    renderSym (HArrIx (IArrComp arr)) = "ArrIx " ++ arr
    renderSym (HArrIx _)              = "ArrIx ..."
    renderSym HBAnd        = "(.&&.)"
    renderSym HBOr         = "(.||.)"
    renderSym HBXor        = "Xor"
    renderSym HBXnor       = "Xnor"
    renderSym HBNand       = "Nand"
    renderSym HBNor        = "Nor"
    renderSym HCond        = error "renderSym-HCond"

    renderArgs = renderArgsSmart

instance StringTree HPrimitive

instance Eval HPrimitive
  where
    evalSym (HFreeVar v) = error $ "evaluating free variable " ++ show v
    evalSym (HLit a)     = a
    evalSym HAdd         = (+)
    evalSym HSub         = (-)
    evalSym HMul         = (*)
    evalSym HNeg         = negate
    evalSym HRem         = rem
    evalSym HDiv         = (/)
    evalSym HPow         = (**)
    evalSym HI2N         = fromInteger . toInteger
    evalSym HI2B         = (/=0)
    evalSym HB2I         = \a -> if a then 1 else 0
    evalSym HNot         = not
    evalSym HAnd         = (&&)
    evalSym HOr          = (||)
    evalSym HEq          = (==)
    evalSym HNEq         = (/=)
    evalSym HLt          = (<)
    evalSym HGt          = (>)
    evalSym HLe          = (<=)
    evalSym HGe          = (>=)
    evalSym (HArrIx (IArrRun arr)) = \i ->
        if i<l || i>h
          then error $ "ArrIx: index "
                    ++ show (toInteger i)
                    ++ " out of bounds "
                    ++ show (toInteger l, toInteger h)
          else arr!i
      where
        (l,h) = bounds arr
    evalSym (HArrIx (IArrComp arr)) = error $ "evaluating symbolic array " ++ arr
    evalSym HBAnd        = (Bits..&.)
    evalSym HBOr         = (Bits..|.)
    evalSym HBXor        = Bits.xor
    evalSym HBXnor       = \a b -> Bits.complement $ Bits.xor a b
    evalSym HBNand       = \a b -> Bits.complement $ (Bits..&.) a b
    evalSym HBNor        = \a b -> Bits.complement $ (Bits..|.) a b
    evalSym HCond        = error "evalSym-HCond"

-- | Assumes no occurrences of 'FreeVar' and concrete representation of arrays
instance EvalEnv HPrimitive env

-- | Returns false for arrays that don't have a symbolic representation
instance Equality HPrimitive
  where
    equal (HFreeVar v) (HFreeVar w) = v==w
    equal (HLit a)     (HLit b)     = show a == show b
    equal HAdd         HAdd         = True
    equal HSub         HSub         = True
    equal HMul         HMul         = True
    equal HNeg         HNeg         = True
    equal HRem         HRem         = True
    equal HDiv         HDiv         = True
    equal HPow         HPow         = True
    equal HI2N         HI2N         = True
    equal HI2B         HI2B         = True
    equal HB2I         HB2I         = True
    equal HNot         HNot         = True
    equal HAnd         HAnd         = True
    equal HOr          HOr          = True
    equal HEq          HEq          = True
    equal HNEq         HNEq         = True
    equal HLt          HLt          = True
    equal HGt          HGt          = True
    equal HLe          HLe          = True
    equal HGe          HGe          = True
    equal (HArrIx (IArrComp arr1)) (HArrIx (IArrComp arr2)) = arr1==arr2
    equal (HArrIx _) (HArrIx _) = False
    equal _ _ = False

--------------------------------------------------------------------------------
