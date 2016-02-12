-- | System interaction in software

module Feldspar.Software
  ( -- * Front end
    module Feldspar
  , module Feldspar.Software.Frontend
    -- * Back ends
  , runIO
  , compile
  , icompile
  , ExternalCompilerOpts (..)
  , defaultExtCompilerOpts
  , compileAndCheck'
  , compileAndCheck
  , runCompiled'
  , runCompiled
  , captureCompiled'
  , captureCompiled
  , compareCompiled'
  , compareCompiled
  ) where



import Language.Embedded.Backend.C (ExternalCompilerOpts (..), defaultExtCompilerOpts)

import Feldspar
import Feldspar.Software.Representation
import Feldspar.Software.Compile
import Feldspar.Software.Frontend

