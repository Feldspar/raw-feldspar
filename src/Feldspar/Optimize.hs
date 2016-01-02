-- | Optimize Feldspar expressions

module Feldspar.Optimize where



import Control.Monad.Writer
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
simplifyUp (AddP t a b@NonLitP)  | isExact a = AddP t b a  -- Move literals to the right
simplifyUp (AddP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b+c))
simplifyUp (AddP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (c-b))
simplifyUp (AddP t a (LitP _ b)) | b < 0, isExact a = SubP t a (LitP t (negate b))

simplifyUp (SubP t (LitP _ 0) b) | isExact b = NegP t b
simplifyUp (SubP t a (LitP _ 0)) | isExact a = a
simplifyUp (SubP t a b@NonLitP) | isExact a = SubP t (NegP t b) (NegP t a)  -- Move literals to the right
simplifyUp (SubP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b-c))
simplifyUp (SubP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = SubP t a (LitP t (b+c))
simplifyUp (SubP t a (LitP _ b)) | b < 0, isExact a = AddP t a (LitP t (negate b))

simplifyUp (MulP t (LitP _ 0) b) | isExact b = LitP t 0
simplifyUp (MulP t a (LitP _ 0)) | isExact a = LitP t 0
simplifyUp (MulP t (LitP _ 1) b) | isExact b = b
simplifyUp (MulP t a (LitP _ 1)) | isExact a = a
simplifyUp (MulP t a b@NonLitP)  | isExact a = MulP t b a  -- Move literals to the right
simplifyUp (MulP t (AddP _ a (LitP _ b)) (LitP _ c)) | isExact a = AddP t a (LitP t (b-c))
simplifyUp (MulP t (SubP _ a (LitP _ b)) (LitP _ c)) | isExact a = SubP t a (LitP t (b+c))
simplifyUp (MulP t a (LitP _ b)) | b < 0, isExact a = AddP t a (LitP t (negate b))

simplifyUp (NegP t (NegP _ a))   | isExact a = a
simplifyUp (NegP t (AddP _ a b)) | isExact a = SubP t (NegP t a) b
simplifyUp (NegP t (SubP _ a b)) | isExact a = SubP t b a
simplifyUp (NegP t (MulP _ a b)) | isExact a = MulP t a (NegP t b)
  -- Negate the right operand, because literals are moved to the right in
  -- multiplications

simplifyUp (SymP t I2N :$ AddP _ a b) | isExact a = AddP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp (SymP t I2N :$ SubP _ a b) | isExact a = SubP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp (SymP t I2N :$ MulP _ a b) | isExact a = MulP t (SymP t I2N :$ a) (SymP t I2N :$ b)
simplifyUp (SymP t I2N :$ NegP _ a)   | isExact a = NegP t (SymP t I2N :$ a)
  -- Pushing down `I2N` is not good for in-exact types, since that puts more of
  -- the expression under the in-exact type. This means that fewer
  -- simplifications may apply. Also, operations on in-exact types are typically
  -- more expensive.

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




simplify :: ASTF FeldDomain a -> ASTF FeldDomain a
simplify = fst . runWriter . go
  where
    go :: ASTF FeldDomain a -> Writer (Set Name) (ASTF FeldDomain a)
    go var
        | Just v <- prVar var = tell (Set.singleton v) >> return var
    go (lam :$ body)
        | Just v <- prLam lam = censor (Set.delete v) $ fmap (lam :$) $ go body
    go a = simpleMatch (\s as -> simplifyUp . appArgs (Sym s) <$> mapArgsM go as) a

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
    sharable (Sym _) _ = False  -- Simple expressions not shared
    sharable (lam :$ _) _
        | Just _ <- prLam lam = False
    sharable _ (lam :$ _)
        | Just _ <- prLam lam = False
      -- Don't place let bindings over lambdas. This ensures that function
      -- arguments of higher-order constructs such as `ForLoop` are always
      -- lambdas.
    sharable (sel :$ _) _
        | Just s <- prj sel, isSelector s = False
            -- Any unary `Tuple` symbol must be a selector (because there are no
            -- 1-tuples).
      where
        isSelector = const True :: Tuple sig -> Bool
    sharable (i2n :$ _) _
        | Just I2N <- prj i2n = False
    sharable (arrIx :$ _) _
        | Just (UnsafeArrIx _) <- prj arrIx = False
    sharable _ _ = True

-- | Optimize a Feldspar expression
optimize :: ASTF FeldDomain a -> ASTF FeldDomain a
optimize = codeMotion cmInterface . simplify

