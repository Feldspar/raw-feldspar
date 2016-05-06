import qualified Prelude

import qualified Data.Complex as Complex

import Feldspar.Run

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import qualified Demo
-- import qualified CoDesign
import qualified Concurrent
import DFT
import FFT

prop_fft_dft dft' fft' = QC.monadicIO $ do
    n   :: Int              <- QC.pick $ QC.choose (2,5)
    inp :: [Complex Double] <- QC.pick $ QC.vector (2 Prelude.^ n)
    outd <- QC.run $ dft' inp
    outf <- QC.run $ fft' inp
    QC.assert (outd Prelude.== outf)

a ~= b = Complex.magnitude d Prelude.< 1e-3 Prelude.&& Complex.phase d Prelude.< 1e-3
  where
    d = abs (a-b)

prop_inverse f fi = QC.monadicIO $ do
    n   :: Int              <- QC.pick $ QC.choose (2,5)
    inp :: [Complex Double] <- QC.pick $ QC.vector (2 Prelude.^ n)
    out1 <- QC.run $ f inp
    out2 <- QC.run $ fi out1
    QC.assert (Prelude.and $ Prelude.zipWith (~=) inp out2)

testFFT =
    marshalledM (return . dft) $ \dft' ->
      marshalledM (return . idft) $ \idft' ->
        marshalledM fft $ \fft' -> do
          marshalledM ifft $ \ifft' -> do
              QC.quickCheck $ prop_fft_dft dft' fft'
              QC.quickCheck $ prop_inverse dft' idft'
              QC.quickCheck $ prop_inverse fft' ifft'
  where
    marshalledM = marshalled' defaultExtCompilerOpts {externalFlagsPost = ["-lm"]}

main = do
    Demo.testAll
--     CoDesign.testAll
    Concurrent.testAll
    testFFT

