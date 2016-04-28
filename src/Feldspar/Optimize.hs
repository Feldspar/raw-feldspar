-- | Optimize Feldspar expressions

module Feldspar.Optimize where



import Control.Monad.Writer hiding (Any (..))
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Constraint (Dict (..))

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Functional.Sharing

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Representation



witInteger :: ASTF FeldDomain a -> Maybe (Dict (Integral a, Ord a))
witInteger a = case getDecor a of
    ValT (Single Int8T)   -> Just Dict
    ValT (Single Int16T)  -> Just Dict
    ValT (Single Int32T)  -> Just Dict
    ValT (Single Int64T)  -> Just Dict
    ValT (Single Word8T)  -> Just Dict
    ValT (Single Word16T) -> Just Dict
    ValT (Single Word32T) -> Just Dict
    ValT (Single Word64T) -> Just Dict
    _ -> Nothing

isExact :: ASTF FeldDomain a -> Bool
isExact = isJust . witInteger

-- | 'prj' with a stronger constraint to allow using it in bidirectional
-- patterns
prj' :: (sub :<: sup) => sup sig -> Maybe (sub sig)
prj' = prj
  -- I think this function wouldn't be needed if one could add an appropriate
  -- type signature for such patterns, but I wan't able to do this for `SymP`
  -- (the inferred type is not accepted).

viewLit :: ASTF FeldDomain a -> Maybe a
viewLit lit
    | Just (Lit a) <- prj lit = Just a
viewLit _ = Nothing

pattern LitP :: (Eq a, Show a) => TypeRep a -> a -> ASTF FeldDomain a
pattern LitP t a <- Sym ((prj -> Just (Lit a)) :&: ValT t)
  where
    LitP t a = Sym (inj (Lit a) :&: ValT t)

pattern NonLitP <- (viewLit -> Nothing)

pattern SymP t s <- Sym ((prj' -> Just s) :&: ValT t)
  where
    SymP t s = Sym ((inj s) :&: ValT t)

pattern VarP t v <- Sym ((prj' -> Just (VarT v)) :&: t)
  where
    VarP t v = Sym (inj (VarT v) :&: t)

pattern LamP t v body <- Sym ((prj' -> Just (LamT v)) :&: t) :$ body
  where
    LamP t v body = Sym (inj (LamT v) :&: t) :$ body

-- There type signatures are needed in order to use `simplifyUp` in the
-- constructor
pattern AddP :: (Num a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a -> ASTF FeldDomain a
pattern SubP :: (Num a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a -> ASTF FeldDomain a
pattern MulP :: (Num a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a -> ASTF FeldDomain a
pattern NegP :: (Num a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a

pattern QuotP :: (Integral a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a -> ASTF FeldDomain a
pattern RemP  :: (Integral a, PrimType' a) => TypeRep a -> ASTF FeldDomain a -> ASTF FeldDomain a -> ASTF FeldDomain a

pattern AddP t a b <- SymP t Add :$ a :$ b where AddP t a b = simplifyUp $ SymP t Add :$ a :$ b
pattern SubP t a b <- SymP t Sub :$ a :$ b where SubP t a b = simplifyUp $ SymP t Sub :$ a :$ b
pattern MulP t a b <- SymP t Mul :$ a :$ b where MulP t a b = simplifyUp $ SymP t Mul :$ a :$ b
pattern NegP t a   <- SymP t Neg :$ a      where NegP t a   = simplifyUp $ SymP t Neg :$ a

pattern QuotP t a b <- SymP t Quot :$ a :$ b where QuotP t a b = simplifyUp $ SymP t Quot :$ a :$ b
pattern RemP t a b  <- SymP t Rem  :$ a :$ b where RemP t a b  = simplifyUp $ SymP t Rem  :$ a :$ b



simplifyUp
    :: ASTF FeldDomain a
    -> ASTF FeldDomain a
simplifyUp (AddP t (LitP _ 0) b) | isExact b = b
simplifyUp (AddP t a (LitP _ 0)) | isExact a = a
simplifyUp (AddP t a@(LitP _ _) b@NonLitP) | isExact a = AddP t b a
  -- Move literals to the right
simplifyUp (AddP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b+c))
simplifyUp (AddP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (c-b))
simplifyUp (AddP t a (LitP _ b)) | Just Dict <- witInteger a, b < 0 = SubP t a (LitP t (negate b))

simplifyUp (SubP t (LitP _ 0) b) | isExact b = NegP t b
simplifyUp (SubP t a (LitP _ 0)) | isExact a = a
simplifyUp (SubP t a@(LitP _ _) b@NonLitP) | isExact a = AddP t (NegP t b) a
  -- Move literals to the right
simplifyUp (SubP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b-c))
simplifyUp (SubP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = SubP t a (LitP t (b+c))
simplifyUp (SubP t a (LitP _ b)) | Just Dict <- witInteger a, b < 0 = AddP t a (LitP t (negate b))

simplifyUp (MulP t (LitP _ 0) b) | isExact b = LitP t 0
simplifyUp (MulP t a (LitP _ 0)) | isExact a = LitP t 0
simplifyUp (MulP t (LitP _ 1) b) | isExact b = b
simplifyUp (MulP t a (LitP _ 1)) | isExact a = a
simplifyUp (MulP t a@(LitP _ _) b@NonLitP) | isExact a = MulP t b a
  -- Move literals to the right
simplifyUp (MulP t (MulP _ a (LitP _ b)) (LitP _ c)) | isExact a = MulP t a (LitP t (b*c))

simplifyUp (NegP t (NegP _ a))   | isExact a = a
simplifyUp (NegP t (AddP _ a b)) | isExact a = SubP t (NegP t a) b
simplifyUp (NegP t (SubP _ a b)) | isExact a = SubP t b a
simplifyUp (NegP t (MulP _ a b)) | isExact a = MulP t a (NegP t b)
  -- Negate the right operand, because literals are moved to the right in
  -- multiplications

simplifyUp (QuotP t (LitP _ 0) b) = LitP t 0
simplifyUp (QuotP _ a (LitP _ 1)) = a
simplifyUp (QuotP t@(Single _) a b) | alphaEq a b = LitP t 1

simplifyUp (RemP t (LitP _ 0) b) = LitP t 0
simplifyUp (RemP t a (LitP _ 1)) = LitP t 0
simplifyUp (RemP t@(Single _) a b) | alphaEq a b = LitP t 0

simplifyUp (SymP _ Not :$ (SymP _ Not :$ a)) = a
simplifyUp (SymP t Not :$ (SymP _ Lt :$ a :$ b)) = simplifyUp $ SymP t Ge :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Gt :$ a :$ b)) = simplifyUp $ SymP t Le :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Le :$ a :$ b)) = simplifyUp $ SymP t Gt :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Ge :$ a :$ b)) = simplifyUp $ SymP t Lt :$ a :$ b

simplifyUp (SymP _ And :$ LitP t False :$ _) = LitP t False
simplifyUp (SymP _ And :$ _ :$ LitP t False) = LitP t False
simplifyUp (SymP _ And :$ LitP t True :$ b)  = b
simplifyUp (SymP _ And :$ a :$ LitP t True)  = a

simplifyUp (SymP _ Or :$ LitP t False :$ b) = b
simplifyUp (SymP _ Or :$ a :$ LitP t False) = a
simplifyUp (SymP _ Or :$ LitP t True :$ _)  = LitP t True
simplifyUp (SymP _ Or :$ _ :$ LitP t True)  = LitP t True

simplifyUp (SymP _ Cond :$ LitP _ True  :$ t :$ _) = t
simplifyUp (SymP _ Cond :$ LitP _ False :$ _ :$ f) = f
simplifyUp (SymP _ Cond :$ c :$ t :$ f) | equal t f = t

-- simplifyUp (SymP _ ForLoop :$ LitP _ 0 :$ init :$ _) = init
  -- This triggers the bug: <https://ghc.haskell.org/trac/ghc/ticket/11336>. The
  -- line below is a workaround:
simplifyUp (Sym ((prj -> Just ForLoop) :&: _) :$ LitP _ 0 :$ init :$ _) = init
simplifyUp (SymP _ ForLoop :$ _ :$ init :$ LamP _ _ (LamP _ vs (VarP _ vs')))
    | vs==vs' = init

simplifyUp (SymP t Pair :$ (SymP _ Fst :$ a) :$ (SymP _ Snd :$ b))
    | alphaEq a b
    , ValT t' <- getDecor a
    , Just Dict <- typeEq t t' = a
simplifyUp (SymP t Fst :$ (SymP _ Pair :$ a :$ _)) = a
simplifyUp (SymP t Snd :$ (SymP _ Pair :$ _ :$ a)) = a
  -- The cases for pairs don't affect the generated code, but they improve the
  -- output of functions like `drawAST`

simplifyUp a = constFold a
  -- `constFold` here ensures that `simplifyUp` does not produce any expressions
  -- that can be statically constant folded. This property is needed, e.g. to
  -- fully simplify the expression `negate (2*x)`. The simplification should go
  -- as follows:
  --
  --     negate (2*x)  ->  negate (x*2)  ->  x * negate 2  ->  x*(-2)
  --
  -- There is no explicit rule for the last step; it is done by `constFold`.
  -- Furthermore, this constant folding would not be performed by `simplifyM`
  -- since it never sees the sub-expression `negate 2`. (Note that the constant
  -- folding in `simplifyM` is still needed, because constructs such as
  -- `ForLoop` cannot be folded by simple literal propagation.)
  --
  -- In order to see that `simplifyUp` doesn't produce any "junk"
  -- (sub-expressions that can be folded by `constFold`), we reason as follows:
  --
  --   * Assume that the arguments of the top-level node are junk-free
  --   * `simplifyUp` will either apply an explicit rewrite or apply `constFold`
  --   * In the latter case, the result will be junk-free
  --   * In case of an explicit rewrite, the resulting expression is constructed
  --     by applying `simplifyUp` to each newly introduced node; thus the
  --     resulting expression must be junk-free



-- | Reduce an expression to a literal if the following conditions are met:
--
-- * The top-most symbol can be evaluated statically (i.g. not a variable or a
--   lifted side-effecting program)
-- * All immediate sub-terms are literals
-- * The type of the expression is an allowed type for literals (e.g. not a
--   function)
--
-- Note that this function only folds the top-level node of an expressions (if
-- possible). It does not reduce an expression like @1+(2+3)@ where the
-- sub-expression @2+3@ is not a literal.
constFold :: ASTF FeldDomain a -> ASTF FeldDomain a
constFold e
    | constArgs e
    , canFold e
    , ValT t@(Single _) <- getDecor e
    = LitP t $ evalClosed e
  where
    canFold :: ASTF FeldDomain a -> Bool
    canFold e = simpleMatch
      (\s _ -> case () of
          _ | SymP _ (FreeVar _) <- e -> False
          _ | SymP _ (ArrIx _) :$ _ <- e -> False
                -- Don't fold array indexing
          _ | SymP _ Pi            <- e -> False
          _ | MulP _ _ (SymP _ Pi) <- e -> False
                -- Don't fold expressions like `2*pi`
          _ | Just (_ :: BindingT sig) <- prj s -> False
          _ | Just (_ :: IOSym sig)    <- prj s -> False
          _ -> True
      )
      e
constFold e = e

-- | Check whether all arguments of a symbol are literals
constArgs :: AST FeldDomain sig -> Bool
constArgs (Sym _)         = True
constArgs (s :$ LitP _ _) = constArgs s
constArgs _               = False



type Opt = Writer (Set Name, Monoid.Any)

tellVar :: Name -> Opt ()
tellVar v = tell (Set.singleton v, mempty)

deleteVar :: Name -> Opt a -> Opt a
deleteVar v = censor (\(vs,unsafe) -> (Set.delete v vs, unsafe))

tellUnsafe :: Opt ()
tellUnsafe = tell (mempty, Monoid.Any True)

simplifyM :: ASTF FeldDomain a -> Opt (ASTF FeldDomain a)
simplifyM a@(VarP _ v)    = tellVar v >> return a
simplifyM (LamP t v body) = deleteVar v $ fmap (LamP t v) $ simplifyM body
simplifyM res@(SymP t I2N :$ AddP _ a b) | isExact res = AddP t <$> simplifyM (SymP t I2N :$ a) <*> simplifyM (SymP t I2N :$ b)
simplifyM res@(SymP t I2N :$ SubP _ a b) | isExact res = SubP t <$> simplifyM (SymP t I2N :$ a) <*> simplifyM (SymP t I2N :$ b)
simplifyM res@(SymP t I2N :$ MulP _ a b) | isExact res = MulP t <$> simplifyM (SymP t I2N :$ a) <*> simplifyM (SymP t I2N :$ b)
simplifyM res@(SymP t I2N :$ NegP _ a)   | isExact res = NegP t <$> simplifyM (SymP t I2N :$ a)
  -- Pushing down `I2N` is not good for in-exact types, since that puts more of
  -- the expression under the in-exact type. This means that fewer
  -- simplifications may apply. Also, operations on in-exact types are typically
  -- more expensive.
  --
  -- Here it's important to guard on whether the *result* is an exact type. (For
  -- other numeric operations it doesn't matter which sub-expression we check
  -- because they all have the same type.)
simplifyM a = simpleMatch
    ( \s@(_ :&: t) as -> do
        (a',(vs, Monoid.Any unsafe)) <- listen (simplifyUp . appArgs (Sym s) <$> mapArgsM simplifyM as)
        case () of
            _ | SymP _ (FreeVar _) <- a' -> tellUnsafe >> return a'
            _ | SymP _ (ArrIx _) :$ _ <- a' -> tellUnsafe >> return a'
                  -- Array indexing is actually not unsafe. It's more like an
                  -- expression with a free variable. But setting the unsafe
                  -- flag does the trick.

            _ | SymP _ Pi <- a' -> return a'
            _ | MulP _ _ (SymP _ Pi) <- a' -> return a'
                  -- Don't fold expressions like `2*pi`

            _ | Just (_ :: IOSym sig) <- prj s -> tellUnsafe >> return a'
            _ | null vs && not unsafe
              , ValT t'@(Single _) <- t
                -> return $ LitP t' $ evalClosed a'
                  -- Constant fold if expression is closed and does not
                  -- contain unsafe operations.
            _ -> return a'
    )
    a

simplify :: ASTF FeldDomain a -> ASTF FeldDomain a
simplify = fst . runWriter . simplifyM

-- | Interface for controlling code motion
cmInterface :: CodeMotionInterface FeldDomain
cmInterface = defaultInterfaceDecor
    typeEqFun
    (\(ValT t)   -> FunT t)
    (\(ValT t)   -> case witTypeable t of Dict -> VarT)
    (\(ValT t) _ -> case witTypeable t of Dict -> LamT)
    sharable
    (const True)
  where
    sharable :: ASTF FeldDomain a -> ASTF FeldDomain b -> Bool
    sharable (Sym _) _      = False  -- Simple expressions not shared
    sharable (LamP _ _ _) _ = False
    sharable _ (LamP _ _ _) = False
      -- Don't place let bindings over lambdas. This ensures that function
      -- arguments of higher-order constructs such as `ForLoop` are always
      -- lambdas.
    sharable (SymP _ (_ :: Tuple (b :-> Full c)) :$ _) _ = False
      -- Any unary `Tuple` symbol must be a selector (because there are no
      -- 1-tuples).
    sharable (SymP _ I2N :$ _) _ = False
    sharable (SymP _ (ArrIx _) :$ _) _ = False
    sharable _ _ = True

-- | Optimize a Feldspar expression
optimize :: ASTF FeldDomain a -> ASTF FeldDomain a
optimize = codeMotion cmInterface . simplify

