{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}

-- | Internal representation of Feldspar programs
module Feldspar.Representation where

import Data.Int
import Data.List (genericTake)
import Data.Typeable (Typeable)
import Data.Word
import Data.Proxy
import Data.Constraint (Dict (..), Constraint)

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import qualified Control.Monad.Operational.Higher as Oper

import qualified Language.Embedded.Expression as Imp
import qualified Language.Embedded.Imperative.CMD as Imp

import qualified Language.Embedded.Hardware.Interface as Hard
import qualified Language.Embedded.Hardware.Expression.Represent.Bit as Hard

import Data.Inhabited
import Data.TypedStruct
import Feldspar.Primitive.Representation

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Object-language types
--------------------------------------------------------------------------------

type family   PrimOf (expr :: * -> *) :: * -> *
type instance PrimOf Data  = Prim
type instance PrimOf HData = HPrim

-- | Representation of all supported types
type TypeRep prim = Struct (PrimType' prim) PrimTypeRep

-- | Supported types
class (Eq a, Show a, Ord a, Typeable a, Inhabited a) => Type expr a
  where
    -- | Reify a type
    typeRep :: proxy expr -> TypeRep (PrimOf expr) a

instance (Type expr a, Type expr b) => Type expr (a, b)
  where
    typeRep p = Two (typeRep p) (typeRep p)

-- | Convert any 'Struct' with a 'PrimType' constraint to a 'TypeRep'
toTypeRep :: forall prim c a. Struct (PrimType' prim) c a -> TypeRep prim a
toTypeRep = mapStruct (const (primTypeRep (Proxy::Proxy prim)))

-- | Check whether two type representations are equal
typeEq :: TypeRep prim a -> TypeRep prim b -> Maybe (Dict (a ~ b))
typeEq (Single t) (Single u) = primTypeEq t u
typeEq (Two t1 t2) (Two u1 u2) = do
    Dict <- typeEq t1 u1
    Dict <- typeEq t2 u2
    return Dict
typeEq _ _ = Nothing

-- | Alias for the conjunction of 'PrimType'' and 'Type'
class    (PrimType' (PrimOf expr) a, Type expr a) => PrimType expr a
instance (PrimType' (PrimOf expr) a, Type expr a) => PrimType expr a

-- | Representation of supported value types + N-ary functions over such types
data TypeRepFun prim a
  where
    ValT :: TypeRep prim a -> TypeRepFun prim a
    FunT :: TypeRep prim a -> TypeRepFun prim b -> TypeRepFun prim (a -> b)
  -- Another option would have been to make `FunT` a constructor in `TypeRep`.
  -- That would have got rid of the extra layer at the expense of less accurate
  -- types (functions would be allowed in pairs, etc.). The current design was
  -- chosen in order to be able to reuse `Struct` instead of making `TypeRep` a
  -- new data type.

-- | Check whether two type representations are equal
typeEqFun :: TypeRepFun prim a -> TypeRepFun prim b -> Maybe (Dict (a ~ b))
typeEqFun (ValT t)     (ValT u)     = typeEq t u
typeEqFun (FunT ta tb) (FunT ua ub) = do
    Dict <- typeEq ta ua
    Dict <- typeEqFun tb ub
    return Dict
typeEqFun _ _ = Nothing

--------------------------------------------------------------------------------

instance Type Data Bool    where typeRep _ = Single BoolT
instance Type Data Int8    where typeRep _ = Single Int8T
instance Type Data Int16   where typeRep _ = Single Int16T
instance Type Data Int32   where typeRep _ = Single Int32T
instance Type Data Int64   where typeRep _ = Single Int64T
instance Type Data Word8   where typeRep _ = Single Word8T
instance Type Data Word16  where typeRep _ = Single Word16T
instance Type Data Word32  where typeRep _ = Single Word32T
instance Type Data Word64  where typeRep _ = Single Word64T
instance Type Data Float   where typeRep _ = Single FloatT
instance Type Data Double  where typeRep _ = Single DoubleT

-- | Reflect a 'TypeRep' to a 'Typeable' constraint.
witTypeable :: TypeRep Prim a -> Dict (Typeable a)
witTypeable (Single t)
    | Dict <- witPrimType t
    = Dict
witTypeable (Two ta tb)
    | Dict <- witTypeable ta
    , Dict <- witTypeable tb
    = Dict

--------------------------------------------------------------------------------

instance Type HData Bool    where typeRep _ = Single BoolT
instance Type HData Int8    where typeRep _ = Single Int8T
instance Type HData Int16   where typeRep _ = Single Int16T
instance Type HData Int32   where typeRep _ = Single Int32T
instance Type HData Int64   where typeRep _ = Single Int64T
instance Type HData Word8   where typeRep _ = Single Word8T
instance Type HData Word16  where typeRep _ = Single Word16T
instance Type HData Word32  where typeRep _ = Single Word32T
instance Type HData Word64  where typeRep _ = Single Word64T
instance Type HData Float   where typeRep _ = Single FloatT
instance Type HData Double  where typeRep _ = Single DoubleT
instance Type HData Integer where typeRep _ = Single IntT
instance Type HData Hard.UBits where typeRep _ = Single UBitsT
instance (Typeable n, KnownNat n) =>
   Type HData (Hard.Bits n) where typeRep _ = Single BitsT

-- | Reflect a 'TypeRep' to a 'Typeable' constraint
witHTypeable :: TypeRep HPrim a -> Dict (Typeable a)
witHTypeable (Single t)
    | Dict <- witHPrimType t
    = Dict
witHTypeable (Two ta tb)
    | Dict <- witHTypeable ta
    , Dict <- witHTypeable tb
    = Dict

--------------------------------------------------------------------------------
{-
-- | Representation of all supported types
type HTypeRep = Struct HPrimType' HPrimTypeRep

-- | Supported types
class (Eq a, Show a, Ord a, Typeable a, Inhabited a) => HType a
  where
    -- | Reify a type
    typeHRep :: HTypeRep a

instance HType Bool    where typeHRep = Single BoolHT
instance HType Int8    where typeHRep = Single Int8HT
instance HType Int16   where typeHRep = Single Int16HT
instance HType Int32   where typeHRep = Single Int32HT
instance HType Int64   where typeHRep = Single Int64HT
instance HType Word8   where typeHRep = Single Word8HT
instance HType Word16  where typeHRep = Single Word16HT
instance HType Word32  where typeHRep = Single Word32HT
instance HType Word64  where typeHRep = Single Word64HT
instance (Typeable n, KnownNat n) => HType (Hard.Bits n) where typeHRep = Single BitsHT
instance HType Integer where typeHRep = Single IntHT
instance (HType a, HType b) => HType (a,b) where typeHRep = Two typeHRep typeHRep

instance Inhabited Integer where
  example = 0

-- | Alias for the conjunction of 'PrimType'' and 'Type'
class    (HPrimType' a, HType a) => HPrimType a
instance (HPrimType' a, HType a) => HPrimType a
-}
{-
-- | Convert any 'Struct' with a 'PrimType' constraint to a 'TypeRep'
toHTypeRep :: Struct HPrimType' c a -> HTypeRep a
toHTypeRep = mapStruct (const primHTypeRep)

-- | Check whether two type representations are equal
typeHEq :: HTypeRep a -> HTypeRep b -> Maybe (Dict (a ~ b))
typeHEq (Single t)  (Single u) = primHTypeEq t u
typeHEq (Two t1 t2) (Two u1 u2) = do
    Dict <- typeHEq t1 u1
    Dict <- typeHEq t2 u2
    return Dict
typeHEq _ _ = Nothing

-- | Reflect a 'TypeRep' to a 'Typeable' constraint
witHTypeable :: HTypeRep a -> Dict (Typeable a)
witHTypeable (Single t)
    | Dict <- witPrimHType t
    = Dict
witHTypeable (Two ta tb)
    | Dict <- witHTypeable ta
    , Dict <- witHTypeable tb
    = Dict

-- | Representation of supported value types + N-ary functions over such types
data HTypeRepFun a
  where
    ValHT :: HTypeRep a -> HTypeRepFun a
    FunHT :: HTypeRep a -> HTypeRepFun b -> HTypeRepFun (a -> b)
  -- Another option would have been to make `FunT` a constructor in `TypeRep`.
  -- That would have got rid of the extra layer at the expense of less accurate
  -- types (functions would be allowed in pairs, etc.). The current design was
  -- chosen in order to be able to reuse `Struct` instead of making `TypeRep` a
  -- new data type.

-- | Check whether two type representations are equal
typeHEqFun :: HTypeRepFun a -> HTypeRepFun b -> Maybe (Dict (a ~ b))
typeHEqFun (ValHT t)     (ValHT u)     = typeHEq t u
typeHEqFun (FunHT ta tb) (FunHT ua ub) = do
    Dict <- typeHEq ta ua
    Dict <- typeHEqFun tb ub
    return Dict
typeHEqFun _ _ = Nothing
-}
--------------------------------------------------------------------------------
{-
type family   TypeRepOf (exp :: * -> *) :: (* -> *)
type instance TypeRepOf Data  = TypeRep
type instance TypeRepOf HData = HTypeRep

type family   TypeRepFunOf (exp :: * -> *) :: (* -> *)
type instance TypeRepFunOf Data  = TypeRepFun
type instance TypeRepFunOf HData = HTypeRepFun

class TypedRep exp
  where
    represent :: TypeOf exp a => proxy exp -> TypeRepOf exp a

instance TypedRep Data where
  represent _ = typeRep

instance TypedRep HData where
  represent _ = typeHRep
-}
--------------------------------------------------------------------------------
{-
type family   TypeOf (exp :: * -> *) :: (* -> Constraint)
type instance TypeOf Data  = Type
type instance TypeOf HData = HType

-- todo: PrimOf
type family   PredOf (exp :: * -> *) :: (* -> Constraint)
type instance PredOf Data  = PrimType'
type instance PredOf HData = HPrimType'
-}

-- | Mutable variable
newtype Ref exp a = Ref { unRef :: Struct (PrimType' (PrimOf exp)) Imp.Ref a }
  -- A reference to a tuple is a struct of smaller references. This means that
  -- creating a reference to a tuple will generate several calls to generate new
  -- references. This must be done already in the front end, which means that
  -- the work in the back end becomes simpler.
  --
  -- Another option would be to allow a single reference to refer to a tuple,
  -- and then turn that into smaller references in the back end. However, this
  -- would complicate the back end for no obvious reason. (One way to do it
  -- would be to run the back end in a store that maps each front end reference
  -- to a struct of small references. Among other things, that would require
  -- dynamic typing.)

-- | Mutable array
newtype Arr exp a = Arr { unArr :: Struct (PrimType' (PrimOf exp)) (Imp.Arr Index) a }
  -- An array of tuples is represented as a struct of smaller arrays. See
  -- comment to `Ref`.

-- | Immutable array
newtype IArr exp a = IArr { unIArr :: Struct (PrimType' (PrimOf exp)) (Imp.IArr Index) a }

--------------------------------------------------------------------------------
-- * Pure expressions
--------------------------------------------------------------------------------

-- | For loop
data ForLoop sig
  where
    ForLoop :: Type Data st =>
        ForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

-- | Interaction with the IO layer
data IOSym sig
  where
    -- Turn a program into a pure value
    UnsafePerform     :: Comp Data (Data a) -> IOSym (Full a)
    -- Identity function with a side effect
    UnsafePerformWith :: Comp Data () -> IOSym (a :-> Full a)

type FeldConstructs
    =   BindingT
    :+: Let
    :+: Tuple
    :+: Primitive
    :+: ForLoop
    :+: IOSym

type FeldDomain = FeldConstructs :&: (TypeRepFun Prim)

newtype Data a = Data { unData :: ASTF FeldDomain a }

-- | Declaring 'Data' as syntactic sugar
instance Syntactic (Data a)
  where
    type Domain   (Data a) = FeldDomain
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

instance Syntactic (Struct (PrimType' Prim) Data a)
    -- Note that this instance places no constraints on `a`. This is crucial in
    -- the way it is used in the rest of the code. It would be possible to
    -- define `desugar` and `sugar` in terms of the instance for pairs; however,
    -- that would require constraining `a`.
  where
    type Domain   (Struct (PrimType' Prim) Data a) = FeldDomain
    type Internal (Struct (PrimType' Prim) Data a) = a

    desugar (Single a) = unData a
    desugar (Two a b)  = sugarSymDecor (ValT $ Two ta tb) Pair a' b'
      where
        a' = desugar a
        b' = desugar b
        ValT ta = getDecor a'
        ValT tb = getDecor b'

    sugar a = case getDecor a of
        ValT (Single _)  -> Single $ Data a
        ValT (Two ta tb) ->
            Two (sugarSymDecor (ValT ta) Fst a) (sugarSymDecor (ValT tb) Snd a)

--------------------------------------------------------------------------------

-- | For loop
data HForLoop sig
  where
    HForLoop :: Type HData st =>
        HForLoop (Length :-> st :-> (Index -> st -> st) :-> Full st)

type HFeldConstructs
    =   BindingT
    :+: Let
    :+: Tuple
    :+: HPrimitive
    :+: HForLoop

type HFeldDomain = HFeldConstructs :&: (TypeRepFun HPrim)

newtype HData a = HData { unHData :: ASTF HFeldDomain a }

-- | Declaring 'HData' as syntactic sugar
instance Syntactic (HData a)
  where
    type Domain   (HData a) = HFeldDomain
    type Internal (HData a) = a
    desugar = unHData
    sugar   = HData

instance Syntactic (Struct (PrimType' HPrim) HData a)
    -- Note that this instance places no constraints on `a`. This is crucial in
    -- the way it is used in the rest of the code. It would be possible to
    -- define `desugar` and `sugar` in terms of the instance for pairs; however,
    -- that would require constraining `a`.
  where
    type Domain   (Struct (PrimType' HPrim) HData a) = HFeldDomain
    type Internal (Struct (PrimType' HPrim) HData a) = a

    desugar (Single a) = unHData a
    desugar (Two a b)  = sugarSymDecor (ValT $ Two ta tb) Pair a' b'
      where
        a' = desugar a
        b' = desugar b
        ValT ta = getDecor a'
        ValT tb = getDecor b'

    sugar a = case getDecor a of
        ValT (Single _)  -> Single $ HData a
        ValT (Two ta tb) ->
            Two (sugarSymDecor (ValT ta) Fst a) (sugarSymDecor (ValT tb) Snd a)

--------------------------------------------------------------------------------

type family DomainOf (exp :: * -> *) :: (* -> *)
type instance DomainOf Data      = FeldDomain
type instance DomainOf HData     = HFeldDomain

type family ExprOf (dat :: k) :: (* -> *)
type instance ExprOf FeldDomain   = Data
type instance ExprOf HFeldDomain  = HData
type instance ExprOf (Data  a)    = Data
type instance ExprOf (HData a)    = HData
type instance ExprOf (Comp exp)   = exp
type instance ExprOf (Comp exp a) = exp
type instance ExprOf (a, b)       = ExprOf a
type instance ExprOf (a, b, c)    = ExprOf a
type instance ExprOf (a, b, c, d) = ExprOf a
type instance ExprOf [a]          = ExprOf a

-- | Specialization of the 'Syntactic' class for the Feldspar domain
class    ( Syntactic a
         , Domain a ~ DomainOf exp
         , Domain a ~ Domain (exp (Internal a))
         , ExprOf (Domain a) ~ exp
         , Type exp (Internal a)

         , SugarSymExp exp
         , SugarSymExpPrim exp

         --, TypedRep exp
         , TypeRep (PrimOf exp) ~ Struct (PrimType' (PrimOf exp)) PrimTypeRep

         , Syntactic (Struct (PrimType' (PrimOf exp)) exp (Internal a))
         , Domain    (Struct (PrimType' (PrimOf exp)) exp (Internal a)) ~ Domain a
         , Internal  (Struct (PrimType' (PrimOf exp)) exp (Internal a)) ~ Internal a
         , Internal  (exp (Internal a)) ~ Internal a
         )
         => Syntax exp a

instance ( Syntactic a
         , Domain a ~ DomainOf exp
         , Domain a ~ Domain (exp (Internal a))
         , ExprOf (Domain a) ~ exp
         , Type exp (Internal a)

         , SugarSymExp exp
         , SugarSymExpPrim exp

         --, TypedRep exp
         , TypeRep (PrimOf exp) ~ Struct (PrimType' (PrimOf exp)) PrimTypeRep

         , Syntactic (Struct (PrimType' (PrimOf exp)) exp (Internal a))
         , Domain    (Struct (PrimType' (PrimOf exp)) exp (Internal a)) ~ Domain a
         , Internal  (Struct (PrimType' (PrimOf exp)) exp (Internal a)) ~ Internal a
         , Internal (exp (Internal a)) ~ Internal a
         )
         => Syntax exp a

--------------------------------------------------------------------------------
{-
type family   PrimOf (exp :: * -> *) :: (* -> *)
type instance PrimOf Data  = PrimDomain
type instance PrimOf HData = HPrimDomain
-}
class SugarSymExp exp
  where
    sugarSymExp
      :: ( Signature sig
         , fi  ~ SmartFun (DomainOf exp) sig
         , sig ~ SmartSig fi
         , DomainOf exp ~ SmartSym fi
         , DomainOf exp ~ (sup :&: TypeRepFun (PrimOf exp))
         , sub :<: sup
         , SyntacticN f fi
         , Type exp (DenResult sig))
      => proxy exp -> sub sig -> f

instance SugarSymExp Data where
  sugarSymExp _ = sugarSymDecor $ ValT $ typeRep (Proxy::Proxy Data)

instance SugarSymExp HData where
  sugarSymExp _ = sugarSymDecor $ ValT $ typeRep (Proxy::Proxy HData)

--------------------------------------------------------------------------------

class SugarSymExpPrim exp
  where
    sugarSymExpPrim
      :: ( Signature sig
         , fi  ~ SmartFun (DomainOf exp) sig
         , sig ~ SmartSig fi
         , DomainOf exp ~ SmartSym fi
         , DomainOf exp ~ (sup :&: TypeRepFun (PrimOf exp))
         , sub :<: sup
         , SyntacticN f fi
         , PrimType' (PrimOf exp) (DenResult sig))
      => proxy exp -> sub sig -> f

instance SugarSymExpPrim Data where
  sugarSymExpPrim _ = sugarSymDecor $ ValT $ Single $ primTypeRep (Proxy::Proxy Prim)

instance SugarSymExpPrim HData where
  sugarSymExpPrim _ = sugarSymDecor $ ValT $ Single $ primTypeRep (Proxy::Proxy HPrim)

-- | Evaluate a closed expression
eval :: (Syntactic a, Domain a ~ FeldDomain) => a -> Internal a
eval = evalClosed . desugar
  -- Note that a `Syntax` constraint would rule out evaluating functions

--------------------------------------------------------------------------------

instance Imp.FreeExp Data
  where
    type FreePred Data = PrimType' Prim
    constExp = sugarSymExpPrim (Proxy::Proxy Data) . Lit
    varExp   = sugarSymExpPrim (Proxy::Proxy Data) . FreeVar

instance Imp.EvalExp Data
  where
    evalExp = eval

instance Imp.FreeExp HData
  where
    type FreePred HData = PrimType' HPrim
    constExp = sugarSymExpPrim (Proxy::Proxy HData) . HLit
    varExp   = sugarSymExpPrim (Proxy::Proxy HData) . HFreeVar

instance Imp.EvalExp HData
  where
    evalExp = error "Imp.evalExp HData" -- eval

--------------------------------------------------------------------------------

instance Hard.FreeExp Data
  where
    type PredicateExp Data = PrimType' Prim
    litE = sugarSymExpPrim (Proxy::Proxy Data) . Lit
    varE = sugarSymExpPrim (Proxy::Proxy Data) . FreeVar

instance Hard.EvaluateExp Data
  where
    evalE = eval

instance Hard.FreeExp HData
  where
    type PredicateExp HData = PrimType' HPrim
    litE = sugarSymExpPrim (Proxy::Proxy HData) . HLit
    varE = sugarSymExpPrim (Proxy::Proxy HData) . HFreeVar

instance Hard.EvaluateExp HData
  where
    evalE = error "Hard.evalE HData" --eval

--------------------------------------------------------------------------------
-- ** ...

class FreeDict exp where
  witPrim :: proxy1 exp -> proxy2 a -> (Imp.FreePred exp a => b) -> (PrimType' (PrimOf exp) a => b)

instance FreeDict Data where
  witPrim :: forall proxy1 proxy2 a b. proxy1 Data -> proxy2 a
    -> (Imp.FreePred Data a => b) -> (PrimType' Prim a => b)
  witPrim p _ f = case freeDict p (primTypeRep (Proxy::Proxy Prim) :: PrimTypeRep a) of
    Dict -> f

instance FreeDict HData where
  witPrim :: forall proxy1 proxy2 a b. proxy1 HData -> proxy2 a
    -> (Imp.FreePred HData a => b) -> (PrimType' HPrim a => b)
  witPrim p _ f = case freeHDict p (primTypeRep (Proxy::Proxy HPrim) :: PrimTypeRep a) of
    Dict -> f

freeDict :: proxy1 Data -> PrimTypeRep a -> Dict (Imp.FreePred Data a)
freeDict _ rep = case rep of
  BoolT   -> Dict
  Int8T   -> Dict
  Int16T  -> Dict
  Int32T  -> Dict
  Int64T  -> Dict
  Word8T  -> Dict
  Word16T -> Dict
  Word32T -> Dict
  Word64T -> Dict
  FloatT  -> Dict
  DoubleT -> Dict
  _       -> error "freeDict-data: invalid type"

freeHDict :: proxy1 HData -> PrimTypeRep a -> Dict (Imp.FreePred HData a)
freeHDict _ rep = case rep of
  BoolT   -> Dict
  Int8T   -> Dict
  Int16T  -> Dict
  Int32T  -> Dict
  Int64T  -> Dict
  Word8T  -> Dict
  Word16T -> Dict
  Word32T -> Dict
  Word64T -> Dict
  IntT    -> Dict
  BitsT   -> Dict
  UBitsT  -> Dict

--------------------------------------------------------------------------------
-- * Monadic computations
--------------------------------------------------------------------------------

type CompCMD = Imp.RefCMD
      Oper.:+: Imp.ArrCMD
      Oper.:+: Imp.ControlCMD

-- | Monad for computational effects: mutable data structures and control flow
newtype Comp (exp :: * -> *) (a :: *) = Comp {
    unComp :: Oper.Program CompCMD (Oper.Param2 exp (PrimType' (PrimOf exp))) a
  }
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

instance Eval ForLoop
  where
    evalSym ForLoop = \len init body ->
        foldl (flip body) init $ genericTake len [0..]

instance Eval IOSym
  where
    evalSym s = error $ "eval: cannot evaluate unsafe operation " ++ renderSym s

instance EvalEnv IOSym env
instance EvalEnv ForLoop env

instance StringTree IOSym
instance StringTree ForLoop

instance Render IOSym
  where
    renderSym (UnsafePerform _)     = "UnsafePerform ..."
    renderSym (UnsafePerformWith _) = "UnsafePerformWith ..."

-- | 'equal' always returns 'False'
instance Equality IOSym
  where
    equal _ _ = False

deriveSymbol    ''ForLoop
deriveRender id ''ForLoop
deriveEquality  ''ForLoop

deriveSymbol ''IOSym

--------------------------------------------------------------------------------

instance Eval HForLoop
  where
    evalSym HForLoop = \len init body ->
      foldl (flip body) init $ genericTake len [0..]

instance EvalEnv HForLoop env

instance StringTree HForLoop

deriveSymbol    ''HForLoop
deriveRender id ''HForLoop
deriveEquality  ''HForLoop

--------------------------------------------------------------------------------
