{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | C code generation of primitive Feldspar expressions

module Feldspar.Primitive.Backend.C where



import Data.Constraint (Dict (..))
import Data.Proxy

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Backend.C

import Language.Syntactic

import Feldspar.Primitive.Representation



instance CompTypeClass (PrimType' Prim)
  where
    compType _ (_ :: proxy a) = case primTypeRep (Proxy::Proxy Prim) :: PrimTypeRep a of
      BoolT   -> addSystemInclude "stdbool.h" >> return [cty| typename bool     |]
      Int8T   -> addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
      Int16T  -> addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
      Int32T  -> addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
      Int64T  -> addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
      Word8T  -> addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
      Word16T -> addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
      Word32T -> addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
      Word64T -> addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
      FloatT  -> addSystemInclude "stdint.h"  >> return [cty| float |]
      DoubleT -> addSystemInclude "stdint.h"  >> return [cty| double |]
      _       -> error "compType-prim: invalid symbol"

    compLit _ a = case primTypeOf (Proxy::Proxy Prim) a of
      BoolT   -> do addSystemInclude "stdbool.h"
                    return $ if a then [cexp| true |] else [cexp| false |]
      Int8T   -> return [cexp| $a |]
      Int16T  -> return [cexp| $a |]
      Int32T  -> return [cexp| $a |]
      Int64T  -> return [cexp| $a |]
      Word8T  -> return [cexp| $a |]
      Word16T -> return [cexp| $a |]
      Word32T -> return [cexp| $a |]
      Word64T -> return [cexp| $a |]
      FloatT  -> return [cexp| $a |]
      DoubleT -> return [cexp| $a |]
      _       -> error "compType-prim: invalid symbol"

-- | Compile a unary operator
compUnOp :: MonadC m => C.UnOp -> ASTF PrimDomain a -> m C.Exp
compUnOp op a = do
    a' <- compPrim $ Prim a
    return $ C.UnOp op a' mempty

-- | Compile a binary operator
compBinOp :: MonadC m =>
    C.BinOp -> ASTF PrimDomain a -> ASTF PrimDomain b -> m C.Exp
compBinOp op a b = do
    a' <- compPrim $ Prim a
    b' <- compPrim $ Prim b
    return $ C.BinOp op a' b' mempty

-- | Compile a function call
compFun :: MonadC m => String -> Args (AST PrimDomain) sig -> m C.Exp
compFun fun args = do
    as <- sequence $ listArgs (compPrim . Prim) args
    return [cexp| $id:fun($args:as) |]

-- | Compile a function call
compCast :: MonadC m => PrimTypeRep a -> ASTF PrimDomain b -> m C.Exp
compCast t a
    | Dict <- witPrimType t = do
        t' <- compType (Proxy :: Proxy (PrimType' Prim)) t
        a' <- compPrim $ Prim a
        return [cexp|($ty:t') $a'|]

-- | Compile an expression
compPrim :: MonadC m => Prim a -> m C.Exp
compPrim = simpleMatch (\(s :&: t) -> go t s) . unPrim
  where
    go :: forall m sig . MonadC m
        => PrimTypeRep (DenResult sig)
        -> Primitive sig
        -> Args (AST PrimDomain) sig
        -> m C.Exp
    go _ (FreeVar v) Nil = touchVar v >> return [cexp| $id:v |]
    go t (Lit a) Nil
        | Dict <- witPrimType t
        = compLit (Proxy :: Proxy (PrimType' Prim)) a
    go _ Pi Nil = addGlobal pi_def >> return [cexp| FELD_PI |]
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

    go _ Cond (c :* t :* f :* Nil) = do
        c' <- compPrim $ Prim c
        t' <- compPrim $ Prim t
        f' <- compPrim $ Prim f
        return $ C.Cond c' t' f' mempty

    --go _ s _ = error $ "compPrim: no handling of symbol " ++ renderSym s
      -- Should not occur, but the completeness checker doesn't know that

instance CompExp Prim where compExp = compPrim

