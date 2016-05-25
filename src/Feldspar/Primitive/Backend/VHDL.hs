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

instance CompileType PrimType'
  where
    compileType _ (_ :: proxy a) = case primTypeRep :: PrimTypeRep a of
      BoolT -> compT (Proxy::Proxy a)
      Int8T -> compT (Proxy::Proxy a)
    compileLit  _ a = case primTypeOf a of
      BoolT -> literal a
      Int8T -> literal a

--------------------------------------------------------------------------------

instance CompileExp Prim
  where
    compE = compP

compP      :: Prim a -> VHDL V.Expression
compP e    = Hoist.lift <$> compSimple e

compLoop   :: ASTF PrimDomain a -> VHDL Kind
compLoop   = compSimple . Prim

compSimple :: Prim a -> VHDL Kind
compSimple = simpleMatch (\(s :&: t) -> compDomain t s) . unPrim
  where
    compDomain :: forall m sig.
         PrimTypeRep (DenResult sig)
      -> Primitive sig
      -> Args (AST PrimDomain) sig
      -> VHDL Kind
    compDomain _ And (a :* b :* Nil) = compExp V.and a b
    compDomain _ Or  (a :* b :* Nil) = compExp V.or  a b
    
    compDomain _ Eq  (a :* b :* Nil) = compRel V.eq  a b
    compDomain _ NEq (a :* b :* Nil) = compRel V.neq a b
    compDomain _ Lt  (a :* b :* Nil) = compRel V.lt  a b
    compDomain _ Gt  (a :* b :* Nil) = compRel V.gt  a b
    compDomain _ Le  (a :* b :* Nil) = compRel V.lte a b
    compDomain _ Ge  (a :* b :* Nil) = compRel V.gte a b
    
    compDomain _ Add (a :* b :* Nil) = compSim V.add a b
    compDomain _ Sub (a :* b :* Nil) = compSim V.sub a b
    compDomain _ Neg (a :* Nil)      = do
      x <- Hoist.lift <$> compLoop a
      return $ Hoist.Si $ V.neg x

    compDomain _ Mul  (a :* b :* Nil) = compTrm V.mul a b
    compDomain _ FDiv (a :* b :* Nil) = compTrm V.div a b
    compDomain _ Rem  (a :* b :* Nil) = compTrm V.rem a b
    compDomain _ Quot (a :* b :* Nil) = error "vhdl support missing for primitive: quot"

    compDomain _ Pow  (a :* b :* Nil) = do
      x <- Hoist.lift <$> compLoop a
      y <- Hoist.lift <$> compLoop b
      return $ Hoist.F $ V.exp x y
    compDomain _ Not  (a :* Nil) = do
      x <- Hoist.lift <$> compLoop a
      return $ Hoist.F $ V.not x

    compDomain _ Sin   (a :* Nil) = error "vhdl support missing for primitive: sin"
    compDomain _ Cos   (a :* Nil) = error "vhdl support missing for primitive: cos"
    compDomain _ Pi    (Nil)      = error "vhdl support missing for primitive: pi"
    compDomain _ Round (a :* Nil) = error "vhdl support missing for primitive: round"

    compDomain _ (FreeVar v) (Nil) =
      return $ Hoist.P $ V.name $ V.NSimple $ V.Ident v
    compDomain t (Lit a)     (Nil) | Dict <- witPrimType t =
      Hoist.E <$> compileLit (Proxy::Proxy PrimType') a

    compDomain _ I2N (a :* Nil) = undefined
    compDomain _ I2B (a :* Nil) = undefined
    compDomain _ B2I (a :* Nil) = undefined

    compDomain _ (ArrIx arr) (i :* Nil) = undefined
    
    compDomain _ Cond (c :* t :* f :* Nil) = undefined
    
-- | ...
compExp
  :: ([V.Relation] -> V.Expression)
  -> ASTF PrimDomain a -> ASTF PrimDomain a
  -> VHDL Kind
compExp op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.E $ op [x, y]

-- | ..
compRel
  :: (V.ShiftExpression -> V.ShiftExpression -> V.Relation)
  -> ASTF PrimDomain a -> ASTF PrimDomain a
  -> VHDL Kind
compRel op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.R $ op x y

-- | ...
compSim
  :: ([V.Term] -> V.SimpleExpression)
  -> ASTF PrimDomain a -> ASTF PrimDomain a
  -> VHDL Kind
compSim op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.Si $ op [x, y]

compTrm
  :: ([V.Factor] -> V.Term)
  -> ASTF PrimDomain a -> ASTF PrimDomain a
  -> VHDL Kind
compTrm op a b = do
  x <- Hoist.lift <$> compLoop a
  y <- Hoist.lift <$> compLoop b
  return $ Hoist.T $ op [x, y]

{-
compP = simpleMatch (\(s :&: t) -> go t s) . unPrim
  where
    go :: forall m sig.
          PrimTypeRep (DenResult sig)
       -> Primitive sig
       -> Args (AST PrimDomain) sig
       -> VHDL V.Expression
    go _ (FreeVar v) Nil = undefined --V.name v
-}
--------------------------------------------------------------------------------
