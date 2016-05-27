{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Syntactic' instances for functions and tuples

module Feldspar.Sugar where

import qualified Language.Haskell.TH as TH

import Data.Proxy
import Data.Typeable (Typeable)
import Data.Constraint (Dict(..))

import Language.Syntactic
import Language.Syntactic.TH
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Functional.Tuple.TH

import Feldspar.Representation
import Data.TypedStruct

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

class TypedRepFun exp
  where
    typeRepVal  :: TypeOf exp a => proxy exp -> TypeRepFunOf exp a
    typeRepFun  :: TypeOf exp a => proxy exp -> TypeRepFunOf exp b -> TypeRepFunOf exp (a -> b)
    typeRepDict :: TypeOf exp a => proxy exp -> Dict (Typeable a)

instance TypedRepFun Data where
  typeRepVal  _ = ValT typeRep
  typeRepFun  _ = FunT typeRep
  typeRepDict _ = Dict

instance TypedRepFun HData where
  typeRepVal  _ = ValHT typeHRep
  typeRepFun  _ = FunHT typeHRep
  typeRepDict _ = Dict

--------------------------------------------------------------------------------

instance forall sup exp a b.
    ( Syntax exp a
    , Syntactic b
    , Domain b ~ DomainOf exp
    , Domain a ~ (sup :&: TypeRepFunOf exp)
    , BindingT :<: sup
    , TypedRepFun exp
    )
    => Syntactic (a -> b)
  where
    type Domain   (a -> b) = Domain a
    type Internal (a -> b) = Internal a -> Internal b
    desugar f = case dict1 of 
        Dict ->
          let varSym v   = inj (VarT v) :&: typeRepVal (Proxy::Proxy exp)
              lamSym v b = Sym (inj (LamT v) :&: typeRepFun (Proxy::Proxy exp) (getDecor b)) :$ b
          in lamT_template varSym lamSym (desugar . f . sugar)
      where
        dict1 :: Dict (Typeable (Internal a))
        dict1 = typeRepDict (Proxy::Proxy exp)
    sugar = error "sugar not implemented for (a -> b)"

instance forall sup exp a b.
    ( Syntax exp a
    , Syntax exp b
    , Domain b ~ DomainOf exp
    , Domain a ~ (sup :&: TypeRepFunOf exp)
    , Tuple :<: sup
      -- ...
    )
    => Syntactic (a, b)
  where
    type Domain   (a, b) = Domain a
    type Internal (a, b) = (Internal a, Internal b)
    desugar (a, b) = undefined
      --sugarSymExp (Proxy::Proxy exp) Pair (desugar a) (desugar b)
    sugar   pair   = undefined
      --( sugarSymExp (Proxy::Proxy exp) Fst pair
      --, sugarSymExp (Proxy::Proxy exp) Snd pair)

{-
instance (Syntax a, Syntactic b, Domain b ~ FeldDomain) => Syntactic (a -> b)
  where
    type Domain (a -> b)   = FeldDomain
    type Internal (a -> b) = Internal a -> Internal b
    desugar f = lamT_template varSym lamSym (desugar . f . sugar)
      where
        varSym v   = inj (VarT v) :&: ValT typeRep
        lamSym v b = Sym (inj (LamT v) :&: FunT typeRep (getDecor b)) :$ b
    sugar = error "sugar not implemented for (a -> b)"
-}{-
instance (Syntax a, Syntax b) => Syntactic (a,b)
  where
    type Domain (a,b)   = FeldDomain
    type Internal (a,b) = (Internal a, Internal b)
    desugar (a,b) = sugarSymFeld Pair (desugar a) (desugar b)
    sugar ab      = (sugarSymFeld Fst ab, sugarSymFeld Snd ab)
-}
--------------------------------------------------------------------------------
{-
deriveSyntacticForTuples
    (return . classPred ''Type TH.ConT . return)
    (\sym -> foldl TH.AppT (TH.ConT ''(:&:)) [sym, TH.ConT ''TypeRepFun])
    [foldl TH.AppT TH.EqualityT
        [TH.VarT (TH.mkName "sym"), TH.ConT ''FeldConstructs]
    ]
    15
-}
--------------------------------------------------------------------------------
