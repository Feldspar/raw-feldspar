-- | System interaction in software

module Feldspar.Software
  ( -- * Front end
    module Feldspar.Software.Frontend
    -- * Back ends
  , runIO
  , compile
  , icompile
  , compile2
  , icompile2
  , compileAndCheck'
  , compileAndCheck
  , runCompiled'
  , runCompiled
  , captureCompiled'
  , captureCompiled
  , compareCompiled'
  , compareCompiled
  ) where



import Feldspar.Software.Representation
import Feldspar.Software.Compile
import Feldspar.Software.Frontend

