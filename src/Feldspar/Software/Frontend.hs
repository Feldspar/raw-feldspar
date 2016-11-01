module Feldspar.Software.Frontend where

-- syntactic.
import Language.Syntactic
import Language.Syntactic.Functional
import qualified Language.Syntactic as Syntactic

import Feldspar.Software.Primitive
import Feldspar.Software.Representation

import Feldspar.Frontend
import Feldspar.Representation

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
type SoftwarePT = Type SoftwarePrimType SoftwarePrimTypeRep

--------------------------------------------------------------------------------

instance Value SoftwarePT SoftwareDomain
  where
    value = sugarSymSoftware . Lit
    
-- todo: Share needs Syntactic instance for (a -> b)

instance Cond SoftwarePT Exp
  where
    cond = sugarSymSoftware Cond

--------------------------------------------------------------------------------
