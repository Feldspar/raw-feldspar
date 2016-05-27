-- | ...

module Feldspar.Primitive.Backend.VHDL where

import Data.Constraint (Dict (..))
import Data.Proxy

import Language.Embedded.Hardware
import Language.Embedded.Hardware.Command.Backend.VHDL
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Hoist (Kind)
import qualified Language.Embedded.Hardware.Expression.Hoist as Hoist

import qualified Language.VHDL          as V
import qualified Language.Embedded.VHDL as V

import Language.Syntactic

import Feldspar.Primitive.Representation

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

instance CompileType HPrimType'
  where
    compileType _ (_ :: proxy a) = case primHTypeRep :: HPrimTypeRep a of
      BoolHT   -> compT (Proxy::Proxy a)
      Int8HT   -> compT (Proxy::Proxy a)
      Int16HT  -> compT (Proxy::Proxy a)
      Int32HT  -> compT (Proxy::Proxy a)
      Int64HT  -> compT (Proxy::Proxy a)
      Word8HT  -> compT (Proxy::Proxy a)
      Word16HT -> compT (Proxy::Proxy a)
      Word32HT -> compT (Proxy::Proxy a)
      Word64HT -> compT (Proxy::Proxy a)
      BitsHT   -> compT (Proxy::Proxy a)
    compileLit  _ a = case primHTypeOf a of
      BoolHT   -> literal a
      Int8HT   -> literal a
      Int16HT  -> literal a
      Int32HT  -> literal a
      Int64HT  -> literal a
      Word8HT  -> literal a
      Word16HT -> literal a
      Word32HT -> literal a
      Word64HT -> literal a
      BitsHT   -> literal a

--------------------------------------------------------------------------------

instance CompileExp HPrim
  where
    compE = compP

compP      :: HPrim a -> VHDL V.Expression
compP e    = Hoist.lift <$> compSimple e

compLoop   :: ASTF HPrimDomain a -> VHDL Kind
compLoop   = compSimple . HPrim

compSimple :: HPrim a -> VHDL Kind
compSimple = simpleMatch (\(s :&: t) -> compDomain t s) . unHPrim
  where
    compDomain :: forall m sig.
         HPrimTypeRep (DenResult sig)
      -> HPrimitive sig
      -> Args (AST HPrimDomain) sig
      -> VHDL Kind
    compDomain _ HAnd (a :* b :* Nil) = compExp V.and a b
    compDomain _ HOr  (a :* b :* Nil) = compExp V.or  a b
    
    compDomain _ HEq  (a :* b :* Nil) = compRel V.eq  a b
    compDomain _ HNEq (a :* b :* Nil) = compRel V.neq a b
    compDomain _ HLt  (a :* b :* Nil) = compRel V.lt  a b
    compDomain _ HGt  (a :* b :* Nil) = compRel V.gt  a b
    compDomain _ HLe  (a :* b :* Nil) = compRel V.lte a b
    compDomain _ HGe  (a :* b :* Nil) = compRel V.gte a b
    
    compDomain _ HAdd (a :* b :* Nil) = compSim V.add a b
    compDomain _ HSub (a :* b :* Nil) = compSim V.sub a b
    compDomain _ HNeg (a :* Nil)      = do
      x <- Hoist.lift <$> compLoop a
      return $ Hoist.Si $ V.neg x

    compDomain _ HMul (a :* b :* Nil) = compTrm V.mul a b
    compDomain _ HDiv (a :* b :* Nil) = compTrm V.div a b
    compDomain _ HRem (a :* b :* Nil) = compTrm V.rem a b

    compDomain _ HPow  (a :* b :* Nil) = do
      x <- Hoist.lift <$> compLoop a
      y <- Hoist.lift <$> compLoop b
      return $ Hoist.F $ V.exp x y
    compDomain _ HNot  (a :* Nil) = do
      x <- Hoist.lift <$> compLoop a
      return $ Hoist.F $ V.not x

    compDomain _ (HFreeVar v) (Nil) =
      return $ Hoist.P $ V.name $ V.NSimple $ V.Ident v
    compDomain t (HLit a)     (Nil) | Dict <- witPrimHType t =
      Hoist.E <$> compileLit (Proxy::Proxy HPrimType') a

    compDomain _ HI2N (a :* Nil) = error "todo: hcompile I2N"
    compDomain _ HI2B (a :* Nil) = error "todo: hcompile I2B"
    compDomain _ HB2I (a :* Nil) = error "todo: hcompile B2I"

    compDomain _ (HArrIx arr) (i :* Nil) = error "todo: hcompile ArrIx"
    
    compDomain _ HCond (c :* t :* f :* Nil) = error "todo: hcompile Cond"
    
-- | ...
compExp
  :: ([V.Relation] -> V.Expression)
  -> ASTF HPrimDomain a -> ASTF HPrimDomain a
  -> VHDL Kind
compExp op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.E $ op [x, y]

-- | ..
compRel
  :: (V.ShiftExpression -> V.ShiftExpression -> V.Relation)
  -> ASTF HPrimDomain a -> ASTF HPrimDomain a
  -> VHDL Kind
compRel op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.R $ op x y

-- | ...
compSim
  :: ([V.Term] -> V.SimpleExpression)
  -> ASTF HPrimDomain a -> ASTF HPrimDomain a
  -> VHDL Kind
compSim op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.Si $ op [x, y]

compTrm
  :: ([V.Factor] -> V.Term)
  -> ASTF HPrimDomain a -> ASTF HPrimDomain a
  -> VHDL Kind
compTrm op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.T $ op [x, y]

--------------------------------------------------------------------------------
