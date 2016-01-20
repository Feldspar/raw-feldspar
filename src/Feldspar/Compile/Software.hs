module Feldspar.Compile.Software where


import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (runIdentity)
import qualified Control.Monad.Reader as Reader

import qualified Control.Monad.Operational.Higher as H

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import Data.TypeRep
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord

import Language.Embedded.CExp (CType)
import qualified Language.Embedded.CExp           as Soft
import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft

import Data.VirtualContainer

import Feldspar.Representation hiding (Program)
import Feldspar.Optimize
import Feldspar.Compile.Lower
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend       as Feld

--------------------------------------------------------------------------------
-- * .
--------------------------------------------------------------------------------

-- | Virtual expression
type VExp = Virtual SmallType Soft.CExp

-- | Virtual expression with hidden result type
data VExp_
  where
    VExp_ :: Type a => Virtual SmallType Soft.CExp a -> VExp_

type TargetCMD =
        Soft.RefCMD     Soft.CExp
  H.:+: Soft.ArrCMD     Soft.CExp
  H.:+: Soft.ControlCMD Soft.CExp

type Env = Map Name VExp_

-- | Target monad for translation
type Target = ReaderT Env (H.Program TargetCMD)

--------------------------------------------------------------------------------
