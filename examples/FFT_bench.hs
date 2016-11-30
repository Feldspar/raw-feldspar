{-# LANGUAGE QuasiQuotes #-}

module FFT_bench where



import Prelude ()

import Feldspar.Run
import Feldspar.Data.Buffered
import Feldspar.Data.Vector

import FFT



sizeOf_double_complex :: Data Length
sizeOf_double_complex = 16
  -- Checked on an x86_64 system
  -- TODO Feldspar should have a built-in `sizeof` function

-- | @2^n@
twoTo :: (Num a, Bits a, PrimType a) => Data Index -> Data a
twoTo n = 1 .<<. i2n n

printTime_def = [cedecl|
void printTime(typename clock_t start, typename clock_t end)
{
  printf("CPU time (sec): %f\n", (double)(end-start) / CLOCKS_PER_SEC);
}
|]

-- | Measure the time for 100 runs of 'fftCore' (excluding initialization) for
-- arrays of the given size
benchmark n = do
    addInclude "<stdio.h>"
    addInclude "<string.h>"
    addInclude "<time.h>"

    addDefinition printTime_def

    start <- newObject "clock_t" False
    end   <- newObject "clock_t" False

    st :: Store (Data (Complex Double)) <- newStore n
    inp <- unsafeFreezeStore n st
    callProc "memset"
      [ iarrArg inp
      , valArg (0 :: Data Index)
      , valArg (n*sizeOf_double_complex)
      ]

    n  <- shareM (ilog2 (length inp))
    ts <- manifestFresh $ Pull (twoTo (n-1)) (tw True (twoTo n))
      -- Change `manifestFresh` to `return` to avoid pre-computing twiddle
      -- factors

    callProcAssign start "clock" []

    for (0,1,Excl 100) $ \(_ :: Data Index) ->
      void $ fftCore st 2 ts n inp

    callProcAssign end "clock" []
    callProc "printTime" [objArg start, objArg end]

runBenchmark n = runCompiled'
    def
    def {externalFlagsPre = ["-O3"], externalFlagsPost = ["-lm"]}
    (benchmark n)

