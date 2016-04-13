{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Primitive Feldspar expressions

module Feldspar.Primitive.Representation where



import Data.Array
import Data.Int
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
    BoolT   :: PrimTypeRep Bool
    Int8T   :: PrimTypeRep Int8
    Int16T  :: PrimTypeRep Int16
    Int32T  :: PrimTypeRep Int32
    Int64T  :: PrimTypeRep Int64
    Word8T  :: PrimTypeRep Word8
    Word16T :: PrimTypeRep Word16
    Word32T :: PrimTypeRep Word32
    Word64T :: PrimTypeRep Word64
    FloatT  :: PrimTypeRep Float
    DoubleT :: PrimTypeRep Double

-- | Primitive supported types
class (Eq a, Ord a, Show a) => PrimType' a
  where
    -- | Reify a primitive type
    primTypeRep :: PrimTypeRep a

instance PrimType' Bool   where primTypeRep = BoolT
instance PrimType' Int8   where primTypeRep = Int8T
instance PrimType' Int16  where primTypeRep = Int16T
instance PrimType' Int32  where primTypeRep = Int32T
instance PrimType' Int64  where primTypeRep = Int64T
instance PrimType' Word8  where primTypeRep = Word8T
instance PrimType' Word16 where primTypeRep = Word16T
instance PrimType' Word32 where primTypeRep = Word32T
instance PrimType' Word64 where primTypeRep = Word64T
instance PrimType' Float  where primTypeRep = FloatT
instance PrimType' Double where primTypeRep = DoubleT

-- | Convenience function; like 'primTypeRep' but with an extra argument to
-- constrain the type parameter. The extra argument is ignored.
primTypeOf :: PrimType' a => a -> PrimTypeRep a
primTypeOf _ = primTypeRep

-- | Check whether two type representations are equal
primTypeEq :: PrimTypeRep a -> PrimTypeRep b -> Maybe (Dict (a ~ b))
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
primTypeEq _ _ = Nothing

-- | Reflect a 'PrimTypeRep' to a 'PrimType'' constraint
witPrimType :: PrimTypeRep a -> Dict (PrimType' a)
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



--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- | Primitive operations
data Primitive sig
  where
    FreeVar :: String -> Primitive (Full a)
    Lit     :: (Eq a, Ord a, Show a) => a -> Primitive (Full a)
    Pi      :: Floating a => Primitive (Full a)

    Add :: Num a => Primitive (a :-> a :-> Full a)
    Sub :: Num a => Primitive (a :-> a :-> Full a)
    Mul :: Num a => Primitive (a :-> a :-> Full a)
    Neg :: Num a => Primitive (a :-> Full a)

    Quot :: Integral a   => Primitive (a :-> a :-> Full a)
    Rem  :: Integral a   => Primitive (a :-> a :-> Full a)
    FDiv :: Fractional a => Primitive (a :-> a :-> Full a)

    Sin :: Floating a => Primitive (a :-> Full a)
    Cos :: Floating a => Primitive (a :-> Full a)
    Pow :: Floating a => Primitive (a :-> a :-> Full a)

    I2N   :: (Integral a, Num b)      => Primitive (a :-> Full b)
    I2B   :: Integral a               => Primitive (a :-> Full Bool)
    B2I   :: Integral a               => Primitive (Bool :-> Full a)
    Round :: (RealFrac a, Integral b) => Primitive (a :-> Full b)

    Not :: Primitive (Bool :-> Full Bool)
    And :: Primitive (Bool :-> Bool :-> Full Bool)
    Or  :: Primitive (Bool :-> Bool :-> Full Bool)
    Eq  :: Eq a  => Primitive (a :-> a :-> Full Bool)
    NEq :: Eq a  => Primitive (a :-> a :-> Full Bool)
    Lt  :: Ord a => Primitive (a :-> a :-> Full Bool)
    Gt  :: Ord a => Primitive (a :-> a :-> Full Bool)
    Le  :: Ord a => Primitive (a :-> a :-> Full Bool)
    Ge  :: Ord a => Primitive (a :-> a :-> Full Bool)

    ArrIx :: IArr Index a -> Primitive (Index :-> Full a)

    Cond :: Primitive (Bool :-> a :-> a :-> Full a)

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

-- | Assumes symbolic representation of arrays
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
    equal (ArrIx _) (ArrIx _) =
        error "equal: can only handle symbolic array representations"
    equal _ _ = False

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

