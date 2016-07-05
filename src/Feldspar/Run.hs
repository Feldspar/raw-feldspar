-- | Monad for running Feldspar programs and C code back ends

module Feldspar.Run
  ( -- * Front end
    module Feldspar
  , module Feldspar.Run.Frontend
  , module Feldspar.Run.Marshal
    -- * Compilation options
  , Selection, select, allExcept, selectBy
  , CompilerOpts (..)
  , ExternalCompilerOpts (..)
  , Default (..)
    -- * Back ends
  , runIO
  , compile'
  , compile
  , compileAll'
  , compileAll
  , icompile'
  , icompile
  , compileAndCheck'
  , compileAndCheck
  , runCompiled'
  , runCompiled
  , withCompiled'
  , withCompiled
  , captureCompiled'
  , captureCompiled
  , compareCompiled'
  , compareCompiled
  ) where



import Language.Embedded.Backend.C (ExternalCompilerOpts (..), Default (..))

import Data.Selection
import Feldspar
import Feldspar.Run.Representation
import Feldspar.Run.Compile
import Feldspar.Run.Frontend
import Feldspar.Run.Marshal

