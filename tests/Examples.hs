{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Prelude

import qualified Data.Complex as Complex

import Feldspar.Run
import Feldspar.Data.Vector
import Feldspar.Data.Buffered

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Tut1_HelloWorld            as Tut1
import qualified Tut2_ExpressionsAndTypes   as Tut2
import qualified Tut3_Vectors               as Tut3
import qualified Tut4_MemoryManagement      as Tut4
import qualified Tut5_Matrices              as Tut5
import qualified Tut6_Testing               as Tut6
import qualified Tut7_ImperativeProgramming as Tut7
import qualified Tut8_SequentialVectors     as Tut8
import qualified Concurrent
import DFT
import FFT



almostEq a b
    =          Complex.magnitude d Prelude.< 1e-7
    Prelude.&& Complex.phase d     Prelude.< 1e-7
  where
    d = abs (a-b)

a ~= b = Prelude.and $ Prelude.zipWith almostEq a b

wrapStore :: (Syntax a, Finite (vec a), MonadComp m) =>
    (Store a -> vec a -> m b) -> vec a -> m b
wrapStore f v = do
    st <- newStore $ length v
    f st v

fftS u = wrapStore (flip fft u)  :: DManifest (Complex Double) -> _
ifftS  = wrapStore (flip ifft 1) :: DManifest (Complex Double) -> _

prop_fft_dft dft' fft' = QC.monadicIO $ do
    n   :: Int              <- QC.pick $ QC.choose (2,5)
    inp :: [Complex Double] <- QC.pick $ QC.vector (2 Prelude.^ n)
    outd <- QC.run $ dft' inp
    outf <- QC.run $ fft' inp
    QC.assert (outd ~= outf)

prop_inverse f fi = QC.monadicIO $ do
    n   :: Int              <- QC.pick $ QC.choose (2,5)
    inp :: [Complex Double] <- QC.pick $ QC.vector (2 Prelude.^ n)
    out1 <- QC.run $ f inp
    out2 <- QC.run $ fi out1
    QC.assert (inp ~= out2)

prop_fib fb1 fb2 = QC.monadicIO $ do
    n   <- QC.pick $ QC.choose (0,40)
    fs1 <- QC.run $ fb1 n
    fs2 <- QC.run $ fb2 n
    QC.assert (fs1 Prelude.== fs2)

main = (Concurrent.testAll >>) $
      -- It doesn't work to have `Concurrent.testAll` as a test case on
      -- GHC 8.4.2. But it works on GHC 8.0.2.

    marshalledM (return . dft)  $ \dft'  ->
    marshalledM (return . idft) $ \idft' ->
    marshalledM (fftS 1)        $ \fft1  ->
    marshalledM (fftS 2)        $ \fft2  ->
    marshalledM ifftS           $ \ifft' ->

    marshalled (return . Tut8.fibSeq) $ \fb1 ->
    marshalled (\n -> return $ Pull n Tut2.fib) $ \fb2 ->

      defaultMain $ testGroup "tests"
        [ testCase "Tut1"       Tut1.testAll
        , testCase "Tut2"       Tut2.testAll
        , testCase "Tut3"       Tut3.testAll
        , testCase "Tut4"       Tut4.testAll
        , testCase "Tut5"       Tut5.testAll
        , testCase "Tut6"       Tut6.testAll
        , testCase "Tut7"       Tut7.testAll
        , testCase "Tut8"       Tut8.testAll
        , testProperty "fft1_dft" $ prop_fft_dft dft' fft1
        , testProperty "fft2_dft" $ prop_fft_dft dft' fft2
        , testProperty "dft_idft" $ prop_inverse dft' idft'
        , testProperty "fft_ifft" $ prop_inverse fft1 ifft'
        , testProperty "fib"      $ prop_fib fb1 fb2
        ]

  where
    marshalledM = marshalled' def def {externalFlagsPost = ["-lm"]}

