module CoDesign where

import qualified Prelude

<<<<<<< HEAD
import Feldspar.Run
import Feldspar.Hardware (liftHardware)
=======
import Feldspar
import Feldspar.Software
import qualified Feldspar.Hardware.Representation as HW
>>>>>>> 9f988ee43bba4624d23de8eeb3a31db82bd97a41
import qualified Feldspar.Hardware.Compile as HW
import qualified Feldspar.Run.Compile as SW

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

simple :: Comp ()
simple = do
  fls <- initRef false
  tru <- initRef true
  end <- initRef (0 :: Data Word32)
  while ((<= 5) <$> getRef end) $ do
    b <- getRef fls
    iff b
      (do setRef tru false
          setRef fls true)
      (do modifyRefD end (+1))

software :: Run ()
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
  printf "The sum of your numbers is %d.\n" (x :: Data Word32)

abort :: Run ()
abort = do
    addInclude "<stdlib.h>"
    callProc "abort" []

--------------------------------------------------------------------------------

testSimple :: IO ()
testSimple = do
  SW.icompile simple
  putStrLn ""
  HW.icompile simple

testSoftware :: IO ()
testSoftware = SW.icompile software

testAll = do
    testSimple
    testSoftware

--------------------------------------------------------------------------------
