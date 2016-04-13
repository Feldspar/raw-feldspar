{-# LANGUAGE QuasiQuotes #-}

-- | C code generation of primitive Feldspar expressions

module Feldspar.Primitive.Backend.C where



import Data.Proxy

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Backend.C

import Language.Syntactic

import Feldspar.Primitive.Representation



instance CompTypeClass PrimType'
  where
    compType _ (_ :: proxy a) = case primTypeRep :: PrimTypeRep a of
      BoolT   -> addSystemInclude "stdbool.h" >> return [cty| typename bool |]
      Int32T  -> addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
      Word32T -> addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]

    compLit _ a = case primTypeOf a of
      BoolT   -> do addSystemInclude "stdbool.h"
                    return $ if a then [cexp| true |] else [cexp| false |]
      Int32T  -> return [cexp| $a |]
      Word32T -> return [cexp| $a |]

-- | Compile a unary operator
compUnOp :: MonadC m
    => C.UnOp
    -> ASTF (Primitive :&: PrimTypeRep) a
    -> m C.Exp
compUnOp op a = do
    a' <- compPrim $ Prim a
    return $ C.UnOp op a' mempty

-- | Compile a binary operator
compBinOp :: MonadC m
    => C.BinOp
    -> ASTF (Primitive :&: PrimTypeRep) a
    -> ASTF (Primitive :&: PrimTypeRep) b
    -> m C.Exp
compBinOp op a b = do
    a' <- compPrim $ Prim a
    b' <- compPrim $ Prim b
    return $ C.BinOp op a' b' mempty

-- | Compile a function call
compFun :: MonadC m
    => String
    -> Args (AST (Primitive :&: PrimTypeRep)) sig
    -> m C.Exp
compFun fun args = do
    as <- sequence $ listArgs (compPrim . Prim) args
    return [cexp| $id:fun($args:as) |]

-- | Compile a function call
compCast :: (PrimType' a, MonadC m)
    => PrimTypeRep a
    -> ASTF (Primitive :&: PrimTypeRep) b
    -> m C.Exp
compCast t a = do
    t' <- compType (Proxy :: Proxy PrimType') t
    a' <- compPrim $ Prim a
    return [cexp|($ty:t') $a'|]

-- | Compile an expression
compPrim :: MonadC m => Prim a -> m C.Exp
compPrim = simpleMatch (\(s :&: t) -> go t s) . unPrim
  where
    go  :: MonadC m
        => PrimTypeRep (DenResult sig)
        -> Primitive sig
        -> Args (AST (Primitive :&: PrimTypeRep)) sig
        -> m C.Exp
    go _ (FreeVar v) Nil = touchVar v >> return [cexp| $id:v |]
    go t (Lit a)     Nil = compLit (Proxy :: Proxy PrimType') a
    go t Pi          Nil = addGlobal pi_def >> return [cexp| FELD_PI |]
      where pi_def = [cedecl|$esc:("#define FELD_PI 3.141592653589793")|]
              -- This is the value of `pi :: Double`.
              -- Apparently there is no standard C99 definition of pi.
    go _ Add  (a :* b :* Nil) = compBinOp C.Add a b
    go _ Sub  (a :* b :* Nil) = compBinOp C.Sub a b
    go _ Mul  (a :* b :* Nil) = compBinOp C.Mul a b
    go _ Neg  (a :* Nil)      = compUnOp C.Negate a
    go _ Quot (a :* b :* Nil) = compBinOp C.Div a b
    go _ Rem  (a :* b :* Nil) = compBinOp C.Mod a b
    go _ FDiv (a :* b :* Nil) = compBinOp C.Div a b
    go _ Not  (a :* Nil)      = compUnOp C.Lnot a
    go _ And  (a :* b :* Nil) = compBinOp C.Land a b
    go _ Or   (a :* b :* Nil) = compBinOp C.Lor a b
    go _ Eq   (a :* b :* Nil) = compBinOp C.Eq a b
    go _ NEq  (a :* b :* Nil) = compBinOp C.Ne a b
    go _ Lt   (a :* b :* Nil) = compBinOp C.Lt a b
    go _ Gt   (a :* b :* Nil) = compBinOp C.Gt a b
    go _ Le   (a :* b :* Nil) = compBinOp C.Le a b
    go _ Ge   (a :* b :* Nil) = compBinOp C.Ge a b

    go _ Sin   args = addInclude "<math.h>" >> compFun "sin" args
    go _ Cos   args = addInclude "<math.h>" >> compFun "cos" args
    go _ Pow   args = addInclude "<math.h>" >> compFun "pow" args
    go _ Round args = addInclude "<math.h>" >> compFun "lround" args

    go t I2N (a :* Nil) = compCast t a
    go t I2B (a :* Nil) = compCast t a
    go t B2I (a :* Nil) = compCast t a

    go _ (ArrIx arr) (i :* Nil) = do
        i' <- compPrim $ Prim i
        touchVar arr
        return [cexp| $id:arr[$i'] |]

instance CompExp Prim where compExp = compPrim

