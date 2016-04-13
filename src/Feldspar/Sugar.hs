{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Syntactic' instances for functions and tuples

module Feldspar.Sugar where



import qualified Language.Haskell.TH as TH

import Language.Syntactic
import Language.Syntactic.TH
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Functional.Tuple.TH

import Feldspar.Representation



instance (Syntax a, Syntactic b, Domain b ~ FeldDomain) => Syntactic (a -> b)
  where
    type Domain (a -> b)   = FeldDomain
    type Internal (a -> b) = Internal a -> Internal b
    desugar f = lamT_template varSym lamSym (desugar . f . sugar)
      where
        varSym v   = inj (VarT v) :&: ValT typeRep
        lamSym v b = Sym (inj (LamT v) :&: FunT typeRep (getDecor b)) :$ b
    sugar = error "sugar not implemented for (a -> b)"

instance (Syntax a, Syntax b) => Syntactic (a,b)
  where
    type Domain (a,b)   = FeldDomain
    type Internal (a,b) = (Internal a, Internal b)
    desugar (a,b) = sugarSymFeld Pair (desugar a) (desugar b)
    sugar ab      = (sugarSymFeld Fst ab, sugarSymFeld Snd ab)

deriveSyntacticForTuples
    (return . classPred ''Type TH.ConT . return)
    (\sym -> foldl TH.AppT (TH.ConT ''(:&:)) [sym, TH.ConT ''TypeRepFun])
    [foldl TH.AppT TH.EqualityT
        [TH.VarT (TH.mkName "sym"), TH.ConT ''FeldConstructs]
    ]
    15

