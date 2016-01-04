-- | Optimize Feldspar expressions

module Feldspar.Optimize where



import Control.Monad.Writer hiding (Any (..))
import qualified Data.Monoid as Monoid
import qualified Data.Typeable as Typeable
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Functional.Sharing

import Data.TypeRep
import Data.TypeRep.Types.Basic

import Feldspar.Representation



isExact :: ASTF FeldDomain a -> Bool
isExact a = simpleMatch
    ( \(_ :&: t) _ -> case () of
          _ | Just _ <- typeEq t floatType  -> False
          _ | Just _ <- typeEq t doubleType -> False
          _ -> True
    )
    a

-- | 'prj' with a stronger constraint to allow using it in bidirectional
-- patterns
prj' :: (sub :<: sup) => sup sig -> Maybe (sub sig)
prj' = prj
  -- I think this function wouldn't be needed if one could add an appropriate
  -- type signature for such patterns, but I wan't able to do this for `SymP`
  -- (the inferred type is not accepted).

viewLit :: ASTF FeldDomain a -> Maybe a
viewLit lit
    | Just (Literal a) <- prj lit = Just a
viewLit _ = Nothing

pattern LitP t a <- Sym ((prj' -> Just (Literal a)) :&: t)
  where
    LitP t a = Sym (inj (Literal a) :&: t)

pattern NonLitP <- (viewLit -> Nothing)

pattern SymP t s <- Sym ((prj' -> Just s) :&: t)
  where
    SymP t s = Sym ((inj s) :&: t)

pattern VarP t v <- Sym ((prj' -> Just (VarT v)) :&: t)
  where
    VarP t v = Sym (inj (VarT v) :&: t)

pattern LamP t v body <- Sym ((prj' -> Just (LamT v)) :&: t) :$ body
  where
    LamP t v body = Sym (inj (LamT v) :&: t) :$ body

pattern AddP t a b <- SymP t Add :$ a :$ b where AddP t a b = SymP t Add :$ a :$ b
pattern SubP t a b <- SymP t Sub :$ a :$ b where SubP t a b = SymP t Sub :$ a :$ b
pattern MulP t a b <- SymP t Mul :$ a :$ b where MulP t a b = SymP t Mul :$ a :$ b
pattern NegP t a   <- SymP t Neg :$ a      where NegP t a   = SymP t Neg :$ a



simplifyUp
    :: ASTF FeldDomain a
    -> ASTF FeldDomain a

simplifyUp (AddP t (LitP _ 0) b) | isExact b = b
simplifyUp (AddP t a (LitP _ 0)) | isExact a = a
simplifyUp (AddP t a@(LitP _ _) b@NonLitP) | isExact a = AddP t b a
  -- Move literals to the right
simplifyUp (AddP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b+c))
simplifyUp (AddP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (c-b))
simplifyUp (AddP t a (LitP _ b)) | b < 0, isExact a = SubP t a (LitP t (negate b))

simplifyUp (SubP t (LitP _ 0) b) | isExact b = NegP t b
simplifyUp (SubP t a (LitP _ 0)) | isExact a = a
simplifyUp (SubP t a@(LitP _ _) b@NonLitP) | isExact a = SubP t (NegP t b) (NegP t a)
  -- Move literals to the right
simplifyUp (SubP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b-c))
simplifyUp (SubP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = SubP t a (LitP t (b+c))
simplifyUp (SubP t a (LitP _ b)) | b < 0, isExact a = AddP t a (LitP t (negate b))

simplifyUp (MulP t (LitP _ 0) b) | isExact b = LitP t 0
simplifyUp (MulP t a (LitP _ 0)) | isExact a = LitP t 0
simplifyUp (MulP t (LitP _ 1) b) | isExact b = b
simplifyUp (MulP t a (LitP _ 1)) | isExact a = a
simplifyUp (MulP t a@(LitP _ _) b@NonLitP) | isExact a = MulP t b a
  -- Move literals to the right
simplifyUp (MulP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b-c))
simplifyUp (MulP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = SubP t a (LitP t (b+c))
simplifyUp (MulP t a (LitP _ b)) | b < 0, isExact a = AddP t a (LitP t (negate b))

simplifyUp (NegP t (NegP _ a))   | isExact a = a
simplifyUp (NegP t (AddP _ a b)) | isExact a = SubP t (NegP t a) b
simplifyUp (NegP t (SubP _ a b)) | isExact a = SubP t b a
simplifyUp (NegP t (MulP _ a b)) | isExact a = MulP t a (NegP t b)
  -- Negate the right operand, because literals are moved to the right in
  -- multiplications

simplifyUp res@(SymP t I2N :$ AddP _ a b) | isExact res = AddP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp res@(SymP t I2N :$ SubP _ a b) | isExact res = SubP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp res@(SymP t I2N :$ MulP _ a b) | isExact res = MulP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp res@(SymP t I2N :$ NegP _ a)   | isExact res = NegP t (SymP t I2N :$ a)
  -- Pushing down `I2N` is not good for in-exact types, since that puts more of
  -- the expression under the in-exact type. This means that fewer
  -- simplifications may apply. Also, operations on in-exact types are typically
  -- more expensive.
  --
  -- Here it's important to guard on whether the *result* is an exact type. (For
  -- other operations it doesn't matter which sub-expression we check because
  -- they all have the same type.)

simplifyUp (SymP _ Not :$ (SymP _ Not :$ a)) = a
simplifyUp (SymP t Not :$ (SymP _ Lt :$ a :$ b)) = SymP t Ge :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Gt :$ a :$ b)) = SymP t Le :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Le :$ a :$ b)) = SymP t Gt :$ a :$ b
simplifyUp (SymP t Not :$ (SymP _ Ge :$ a :$ b)) = SymP t Lt :$ a :$ b

simplifyUp (SymP _ Condition :$ LitP _ True  :$ t :$ _) = t
simplifyUp (SymP _ Condition :$ LitP _ False :$ _ :$ f) = f
simplifyUp (SymP _ Condition :$ c :$ t :$ f) | equal t f = t

-- simplifyUp (SymP _ ForLoop :$ LitP _ 0 :$ init :$ _) = init
  -- This triggers the bug: <https://ghc.haskell.org/trac/ghc/ticket/11336>. The
  -- line below is a workaround:
simplifyUp (Sym ((prj -> Just ForLoop) :&: _) :$ LitP _ 0 :$ init :$ _) = init
simplifyUp (SymP _ ForLoop :$ _ :$ init :$ LamP _ _ (LamP _ vs (VarP _ vs')))
    | vs==vs' = init

simplifyUp a = a



type Opt = Writer (Set Name, Monoid.Any)

tellVar :: Name -> Opt ()
tellVar v = tell (Set.singleton v, mempty)

deleteVar :: Name -> Opt a -> Opt a
deleteVar v = censor (\(vs,unsafe) -> (Set.delete v vs, unsafe))

tellUnsafe :: Opt ()
tellUnsafe = tell (mempty, Monoid.Any True)

simplify :: ASTF FeldDomain a -> ASTF FeldDomain a
simplify = fst . runWriter . go
  where
    go :: ASTF FeldDomain a -> Opt (ASTF FeldDomain a)
    go a@(VarP _ v)    = tellVar v >> return a
    go (LamP t v body) = deleteVar v $ fmap (LamP t v) $ go body
    go a = simpleMatch
      ( \s@(_ :&: t) as -> do
          (a',(vs, Monoid.Any unsafe)) <- listen (simplifyUp . appArgs (Sym s) <$> mapArgsM go as)
          case prj s of
              Just (_ :: IOSym sig) -> tellUnsafe >> return a'
              _ | null vs && not unsafe
                , Right Dict <- pwit pShow t
                  -> return $ LitP t $ evalClosed a'
                    -- Constant fold if expression is closed and does not
                    -- contain unsafe operations.
              _ -> return a'
      )
      a

-- | Interface for controlling code motion
cmInterface :: CodeMotionInterface FeldDomain
cmInterface = defaultInterfaceDecor
    typeEq
    funType
    (\t   -> case wit pTypeable t of Dict -> VarT)
    (\t _ -> case wit pTypeable t of Dict -> LamT)
    sharable
    (const True)
  where
    pTypeable :: Proxy Typeable.Typeable
    pTypeable = Proxy

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
    sharable (SymP _ (UnsafeArrIx _) :$ _) _ = False
    sharable _ _ = True

-- | Optimize a Feldspar expression
optimize :: ASTF FeldDomain a -> ASTF FeldDomain a
optimize = codeMotion cmInterface . simplify

