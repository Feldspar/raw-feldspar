-- | Interface to the FFTW routine
--
-- Ubuntu installation of C library:
--
--     sudo apt-get install libfftw3-dev
--
-- Basic usage:
-- <http://www.fftw.org/fftw3_doc/Complex-One_002dDimensional-DFTs.html#Complex-One_002dDimensional-DFTs>
--
-- Linker flags:
--
--     cc file.c -lm -lfftw3

module FFTW where



import qualified Prelude as P

import qualified Data.Complex as Complex

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import Language.Embedded.Expression (varExp)
  -- `varExp` can be used to make named constants in expressions

import Feldspar.Run
import Feldspar.Data.Vector

import DFT
import FFT_bench (printTime_def)



-- | Wrapper for the FFTW routine
--
-- This wrapper is mainly used for testing. It's not suitable for real code
-- because it constructs a plan every time it's called, and it silently
-- allocates an array for the output.
fftw :: DManifest (Complex Double) -> Run (DManifest (Complex Double))
fftw inp = do
    addInclude "<fftw3.h>"
    out  <- newArr (length inp)
    plan <- newObject "fftw_plan" False
    callProcAssign plan "fftw_plan_dft_1d"
      [ valArg (length inp)
      , iarrArg inp
      , arrArg out
      , valArg (varExp "FFTW_FORWARD"  :: Data Word32)
      , valArg (varExp "FFTW_ESTIMATE" :: Data Word32)
      ]
    callProc "fftw_execute" [objArg plan]
    freezeArr out



almostEq a b
    =    Complex.magnitude d P.< 1e-7
    P.&& Complex.phase d     P.< 1e-7
  where
    d = abs (a-b)

a ~= b = P.and $ P.zipWith almostEq a b

prop_fft_dft dft' fft' = QC.monadicIO $ do
    n   :: Int              <- QC.pick $ QC.choose (2,5)
    inp :: [Complex Double] <- QC.pick $ QC.vector (2 P.^ n)
    outd <- QC.run $ dft' inp
    outf <- QC.run $ fft' inp
    QC.assert (outd ~= outf)

-- | Compare 'fftw' against 'dft'
testFFTW =
    marshalledM (return . dft) $ \dft'  ->
    marshalledM fftw           $ \fftw' ->
      QC.quickCheck $ prop_fft_dft dft' fftw'
  where
    marshalledM = marshalled' def def
      { externalFlagsPre  = ["-Wno-incompatible-pointer-types"]
      , externalFlagsPost = ["-lm -lfftw3"]
      }



sizeOf_fftw_complex :: Data Length
sizeOf_fftw_complex = 16
  -- Checked on an x86_64 system

-- | Measure the time for 100 runs of 'fftw' (excluding initialization) for
-- arrays of the given size
benchmark n = do
    addInclude "<stdio.h>"
    addInclude "<string.h>"
    addInclude "<time.h>"
    addInclude "<fftw3.h>"

    addDefinition printTime_def

    inp  <- newObject "fftw_complex" True
    out  <- newObject "fftw_complex" True
    plan <- newObject "fftw_plan" False

    callProcAssign inp "fftw_malloc" [valArg (n*sizeOf_fftw_complex)]
    callProcAssign out "fftw_malloc" [valArg (n*sizeOf_fftw_complex)]
    callProc "memset"
      [ objArg inp
      , valArg (0 :: Data Index)
      , valArg (n*sizeOf_fftw_complex)
      ]

    callProcAssign plan "fftw_plan_dft_1d"
      [ valArg (n :: Data Word32)
      , objArg inp
      , objArg out
      , valArg (varExp "FFTW_FORWARD"  :: Data Word32)
      , valArg (varExp "FFTW_ESTIMATE" :: Data Word32)
          -- Change to `FFTW_MEASURE` to enable tuning
      ]

    start <- newObject "clock_t" False
    end   <- newObject "clock_t" False
    callProcAssign start "clock" []

    for (0,1,Excl 100) $ \(_ :: Data Index) ->
      callProc "fftw_execute" [objArg plan]

    callProcAssign end "clock" []
    callProc "printTime" [objArg start, objArg end]

    callProc "fftw_destroy_plan" [objArg plan]
    callProc "fftw_free" [objArg inp]
    callProc "fftw_free" [objArg out]

runBenchmark n = runCompiled'
    def
    def {externalFlagsPre = ["-O3"], externalFlagsPost = ["-lm","-lfftw3"]}
    (benchmark n)

