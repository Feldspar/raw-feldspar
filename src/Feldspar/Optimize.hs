-- | Optimize Feldspar expressions

module Feldspar.Optimize where



import qualified Data.Typeable as Typeable

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Functional.Sharing

import Data.TypeRep
import Data.TypeRep.Types.Basic

import Feldspar.Representation



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
    sharable (Sym _) _ = False
      -- Simple expressions not shared
    sharable (lam :$ _) _
        | Just _ <- prLam lam = False
      -- Lambdas not shared
    sharable _ (lam :$ _)
        | Just _ <- prLam lam = False
      -- Don't place let bindings over lambdas. This ensures that function
      -- arguments of higher-order constructs such as `ForLoop` are always
      -- lambdas.
    sharable (sel :$ _) _
        | Just s <- prj sel, isSelector s = False
            -- Any unary `Tuple` symbol must be a selector (because there are no
            -- 1-tuples.
      where
        isSelector = const True :: Tuple sig -> Bool
      -- Tuple selection not shared
    sharable (i2n :$ _) _
        | Just I2N <- prj i2n = False
      -- Type casts not shared
    sharable (arrIx :$ _) _
        | Just (UnsafeArrIx _) <- prj arrIx = False
      -- Array length not shared
    sharable _ _ = True

-- | Optimize a Feldspar expression
optimize :: ASTF FeldDomain a -> ASTF FeldDomain a
optimize = codeMotion cmInterface

