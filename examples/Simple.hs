module Simple where

import Control.Applicative ((<$>))
import Control.Monad.Trans

import Prelude ((.), ($))
import qualified Prelude

import Feldspar
import Feldspar.Representation (unProgram)
import qualified Feldspar.Compile.Hardware as HW
import qualified Feldspar.Compile.Software as SW

import Language.Embedded.Imperative.Frontend.General (stdin, stdout)
import Language.Embedded.Hardware.Command (wcompile)

import System.IO hiding (stdin, stdout)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

simple :: Program ()
simple = do
  fls <- initRef false
  tru <- initRef true
  end <- initRef (0 :: Data Word32)
  while ((<= 5) <$> getRef end) $ do
    b <- getRef fls
    iff b
      (do setRef tru false
          setRef fls true)
      (do modifyRef end (+1))

software :: Software ()
software = do
  done <- initRef false
  sum  <- initRef (0 :: Data Word32)
  n    <- initRef (0 :: Data Word8)
  while (not <$> getRef done) $ do
    printf "Enter a number (0 means done): "
    n <- fget stdin
    iff (n == 0)
      (setRef done true)
      (modifyRef sum (+n))
  x <- getRef sum
  printf "The sum of your numbers is %d.\n" x

abort :: Software ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

--------------------------------------------------------------------------------

testSimple :: IO ()
testSimple = do
  SW.icompile simple
  putStrLn ""
  wcompile (HW.lowerTop simple)

testSoftware :: IO ()
testSoftware = SW.icompile2 software

--------------------------------------------------------------------------------
