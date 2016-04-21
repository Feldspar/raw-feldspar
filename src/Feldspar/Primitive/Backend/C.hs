{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | C code generation of primitive Feldspar expressions

module Feldspar.Primitive.Backend.C where



import Data.Complex

import Data.Constraint (Dict (..))
import Data.Proxy

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Language.C.Monad
import Language.Embedded.Backend.C

import Language.Syntactic

import Feldspar.Primitive.Representation



-- Note: This module assumes a 32-bit target. For example the C function `abs`
-- is used up to 32-bits, and `labs` is used above that.

instance CompTypeClass PrimType'
  where
    compType _ (_ :: proxy a) = case primTypeRep :: PrimTypeRep a of
      BoolT   -> addInclude "<stdbool.h>" >> return [cty| typename bool     |]
      Int8T   -> addInclude "<stdint.h>"  >> return [cty| typename int8_t   |]
      Int16T  -> addInclude "<stdint.h>"  >> return [cty| typename int16_t  |]
      Int32T  -> addInclude "<stdint.h>"  >> return [cty| typename int32_t  |]
      Int64T  -> addInclude "<stdint.h>"  >> return [cty| typename int64_t  |]
      Word8T  -> addInclude "<stdint.h>"  >> return [cty| typename uint8_t  |]
      Word16T -> addInclude "<stdint.h>"  >> return [cty| typename uint16_t |]
      Word32T -> addInclude "<stdint.h>"  >> return [cty| typename uint32_t |]
      Word64T -> addInclude "<stdint.h>"  >> return [cty| typename uint64_t |]
      FloatT  -> return [cty| float |]
      DoubleT -> return [cty| double |]
      ComplexFloatT  -> addInclude "<tgmath.h>" >> return [cty| float  _Complex |]
      ComplexDoubleT -> addInclude "<tgmath.h>" >> return [cty| double _Complex |]

    compLit _ a = case primTypeOf a of
      BoolT   -> do addInclude "<stdbool.h>"
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
      ComplexFloatT  -> return $ compComplexLit a
      ComplexDoubleT -> return $ compComplexLit a

-- | Compile a complex literal
compComplexLit :: (Eq a, Num a, ToExp a) => Complex a -> C.Exp
compComplexLit (r :+ 0) = [cexp| $r |]
compComplexLit (0 :+ i) = [cexp| $i * I |]
compComplexLit (r :+ i) = [cexp| $r + $i * I |]

addTagMacro :: MonadC m => m ()
addTagMacro = addGlobal [cedecl|$esc:("#define TAG(tag,exp) (exp)")|]

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

-- | Compile a call to 'abs'
compAbs :: MonadC m => PrimTypeRep a -> ASTF PrimDomain a -> m C.Exp
compAbs t a = do
    addInclude "<tgmath.h>"
    case t of
        BoolT          -> error "compAbs: type BoolT not supported"
        Int8T          -> compFun "abs"  (a :* Nil)
        Int16T         -> compFun "abs"  (a :* Nil)
        Int32T         -> compFun "abs"  (a :* Nil)
        Int64T         -> compFun "labs" (a :* Nil)
        FloatT         -> compFun "fabs" (a :* Nil)
        DoubleT        -> compFun "fabs" (a :* Nil)
        ComplexFloatT  -> compFun "fabs" (a :* Nil)
        ComplexDoubleT -> compFun "fabs" (a :* Nil)
        _ -> compPrim $ Prim a  -- Unsigned integers

complexSign_def = [cedecl|
double _Complex feld_complexSign(double _Complex c) {
    double z = cabs(c);
    if (z == 0) {
        return 0;
    } else {
        return (creal(c)/z + I*(cimag(c)/z));
    }
}
|]

complexSignf_def = [cedecl|
float _Complex feld_complexSignf(float _Complex c) {
    float z = cabsf(c);
    if (z == 0) {
        return 0;
    } else {
        return (crealf(c)/z + I*(cimagf(c)/z));
    }
}
|]

-- | Compile a call to 'signum'
compSign :: MonadC m => PrimTypeRep a -> ASTF PrimDomain a -> m C.Exp
compSign t a = case viewPrimTypeRep t of
    PrimTypeBool -> error "compSign: type BoolT not supported"
    PrimTypeIntWord (WordType _) -> do
        addTagMacro
        a' <- compPrim $ Prim a
        return [cexp| TAG("signum", $a' > 0) |]
    PrimTypeIntWord (IntType _) -> do
        addTagMacro
        a' <- compPrim $ Prim a
        return [cexp| TAG("signum", ($a' > 0) - ($a' < 0)) |]
    PrimTypeFloating FloatType -> do
        addTagMacro
        a' <- compPrim $ Prim a
        return [cexp| TAG("signum", (float) (($a' > 0) - ($a' < 0))) |]
    PrimTypeFloating DoubleType -> do
        addTagMacro
        a' <- compPrim $ Prim a
        return [cexp| TAG("signum", (double) (($a' > 0) - ($a' < 0))) |]
    PrimTypeComplex ComplexDoubleType -> do
        addInclude "<tgmath.h>"
        addGlobal complexSign_def
        a' <- compPrim $ Prim a
        return [cexp| feld_complexSign($a') |]
    PrimTypeComplex ComplexFloatType -> do
        addInclude "<complex.h>"
        addGlobal complexSignf_def
        a' <- compPrim $ Prim a
        return [cexp| feld_complexSignf($a') |]
  -- TODO The floating point cases give `sign (-0.0) = 0.0`, which is (slightly)
  -- wrong. They should return -0.0. I don't know whether it's correct for other
  -- strange values.

-- | Compile a function call
compCast :: MonadC m => PrimTypeRep a -> ASTF PrimDomain b -> m C.Exp
compCast t a
    | Dict <- witPrimType t = do
        t' <- compType (Proxy :: Proxy PrimType') t
        a' <- compPrim $ Prim a
        return [cexp|($ty:t') $a'|]

-- | Compile a call to 'round'
compRound :: (Integral a, RealFrac b, MonadC m) =>
    PrimTypeRep a -> ASTF PrimDomain b -> m C.Exp
compRound t a = do
    addInclude "<tgmath.h>"
    case primTypeIntWidth t of
        Just w | w < 64 -> compFun "round" (a :* Nil)
        Just w          -> compFun "lround" (a :* Nil)
        _ -> error $ "compRound: type " ++ show t ++ " not supported"

-- Note: There's no problem with including both `tgmath.h` and `math.h`. As long
-- as the former is included, including the latter (before or after) doesn't
-- make a difference.
--
-- See: <https://gist.github.com/emilaxelsson/51310b3353f96914cd9bdb18b10b3103>

div_def = [cedecl|
int feld_div(int x, int y) {
    int q = x/y;
    int r = x%y;
    if ((r!=0) && ((r<0) != (y<0))) --q;
    return q;
}
|]

ldiv_def = [cedecl|
long int feld_ldiv(long int x, long int y) {
    int q = x/y;
    int r = x%y;
    if ((r!=0) && ((r<0) != (y<0))) --q;
    return q;
}
|]

mod_def = [cedecl|
int feld_mod(int x, int y) {
    int r = x%y;
    if ((r!=0) && ((r<0) != (y<0))) { r += y; }
    return r;
}
|]

lmod_def = [cedecl|
long int feld_lmod(long int x, long int y) {
    int r = x%y;
    if ((r!=0) && ((r<0) != (y<0))) { r += y; }
    return r;
}
|]

-- The above C implementations are taken from
-- <http://www.microhowto.info/howto/round_towards_minus_infinity_when_dividing_integers_in_c_or_c++.html>

compDiv :: MonadC m =>
    PrimTypeRep a -> ASTF PrimDomain a -> ASTF PrimDomain b -> m C.Exp
compDiv t a b = case primTypeIntWidth t of
    Just w | w < 64 -> do
        addGlobal div_def
        compFun "feld_div" (a :* b :* Nil)
    Just w -> do
        addGlobal ldiv_def
        compFun "feld_ldiv" (a :* b :* Nil)
    _ -> error $ "compDiv: type " ++ show t ++ " not supported"

compMod :: MonadC m =>
    PrimTypeRep a -> ASTF PrimDomain a -> ASTF PrimDomain b -> m C.Exp
compMod t a b = case primTypeIntWidth t of
    Just w | w < 64 -> do
        addGlobal mod_def
        compFun "feld_mod" (a :* b :* Nil)
    Just w -> do
        addGlobal lmod_def
        compFun "feld_lmod" (a :* b :* Nil)
    _ -> error $ "compMod: type " ++ show t ++ " not supported"

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
        = compLit (Proxy :: Proxy PrimType') a
    go _ Add  (a :* b :* Nil) = compBinOp C.Add a b
    go _ Sub  (a :* b :* Nil) = compBinOp C.Sub a b
    go _ Mul  (a :* b :* Nil) = compBinOp C.Mul a b
    go _ Neg  (a :* Nil)      = compUnOp C.Negate a
    go t Abs  (a :* Nil)      = compAbs t a
    go t Sign (a :* Nil)      = compSign t a

    go _ Quot (a :* b :* Nil) = compBinOp C.Div a b
    go _ Rem  (a :* b :* Nil) = compBinOp C.Mod a b
    go t Div  (a :* b :* Nil) = compDiv t a b
    go t Mod  (a :* b :* Nil) = compMod t a b
    go _ FDiv (a :* b :* Nil) = compBinOp C.Div a b

    go _ Pi Nil = addGlobal pi_def >> return [cexp| FELD_PI |]
      where pi_def = [cedecl|$esc:("#define FELD_PI 3.141592653589793")|]
              -- This is the value of `pi :: Double`.
              -- Apparently there is no standard C99 definition of pi.
    go _ Exp   args = addInclude "<tgmath.h>" >> compFun "exp" args
    go _ Log   args = addInclude "<tgmath.h>" >> compFun "log" args
    go _ Sqrt  args = addInclude "<tgmath.h>" >> compFun "sqrt" args
    go _ Pow   args = addInclude "<tgmath.h>" >> compFun "pow" args
    go _ Sin   args = addInclude "<tgmath.h>" >> compFun "sin" args
    go _ Cos   args = addInclude "<tgmath.h>" >> compFun "cos" args
    go _ Tan   args = addInclude "<tgmath.h>" >> compFun "tan" args
    go _ Asin  args = addInclude "<tgmath.h>" >> compFun "asin" args
    go _ Acos  args = addInclude "<tgmath.h>" >> compFun "acos" args
    go _ Atan  args = addInclude "<tgmath.h>" >> compFun "atan" args
    go _ Sinh  args = addInclude "<tgmath.h>" >> compFun "sinh" args
    go _ Cosh  args = addInclude "<tgmath.h>" >> compFun "cosh" args
    go _ Tanh  args = addInclude "<tgmath.h>" >> compFun "tanh" args
    go _ Asinh args = addInclude "<tgmath.h>" >> compFun "asinh" args
    go _ Acosh args = addInclude "<tgmath.h>" >> compFun "acosh" args
    go _ Atanh args = addInclude "<tgmath.h>" >> compFun "atanh" args

    go _ Real      args = addInclude "<tgmath.h>" >> compFun "creal"  args
    go _ Imag      args = addInclude "<tgmath.h>" >> compFun "cimag"  args
    go _ Magnitude args = addInclude "<tgmath.h>" >> compFun "cabs"   args
    go _ Phase     args = addInclude "<tgmath.h>" >> compFun "carg"   args
    go _ Conjugate args = addInclude "<tgmath.h>" >> compFun "conj"   args

    go t I2N   (a :* Nil) = compCast t a
    go t I2B   (a :* Nil) = compCast t a
    go t B2I   (a :* Nil) = compCast t a
    go t Round (a :* Nil) = compRound t a

    go _ Not  (a :* Nil)      = compUnOp C.Lnot a
    go _ And  (a :* b :* Nil) = compBinOp C.Land a b
    go _ Or   (a :* b :* Nil) = compBinOp C.Lor a b
    go _ Eq   (a :* b :* Nil) = compBinOp C.Eq a b
    go _ NEq  (a :* b :* Nil) = compBinOp C.Ne a b
    go _ Lt   (a :* b :* Nil) = compBinOp C.Lt a b
    go _ Gt   (a :* b :* Nil) = compBinOp C.Gt a b
    go _ Le   (a :* b :* Nil) = compBinOp C.Le a b
    go _ Ge   (a :* b :* Nil) = compBinOp C.Ge a b

    go _ BitAnd   (a :* b :* Nil) = compBinOp C.And a b
    go _ BitOr    (a :* b :* Nil) = compBinOp C.Or a b
    go _ BitXor   (a :* b :* Nil) = compBinOp C.Xor a b
    go _ BitCompl (a :* Nil)      = compUnOp C.Not a
    go _ ShiftL   (a :* b :* Nil) = compBinOp C.Lsh a b
    go _ ShiftR   (a :* b :* Nil) = compBinOp C.Rsh a b

    go _ (ArrIx arr) (i :* Nil) = do
        i' <- compPrim $ Prim i
        touchVar arr
        return [cexp| $id:arr[$i'] |]

    go _ Cond (c :* t :* f :* Nil) = do
        c' <- compPrim $ Prim c
        t' <- compPrim $ Prim t
        f' <- compPrim $ Prim f
        return $ C.Cond c' t' f' mempty

    go _ s _ = error $ "compPrim: no handling of symbol " ++ renderSym s
      -- Should not occur, but the completeness checker doesn't know that

instance CompExp Prim where compExp = compPrim

