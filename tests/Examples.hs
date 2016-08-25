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

fftS  = wrapStore fft  :: DManifest (Complex Double) -> _
ifftS = wrapStore ifft :: DManifest (Complex Double) -> _

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

main =
    marshalledM (return . dft)  $ \dft'  ->
    marshalledM (return . idft) $ \idft' ->
    marshalledM fftS            $ \fft'  ->
    marshalledM ifftS           $ \ifft' ->

      defaultMain $ testGroup "tests"
        [ testCase "Tut1"       Tut1.testAll
        , testCase "Tut2"       Tut2.testAll
        , testCase "Tut3"       Tut3.testAll
        , testCase "Tut4"       Tut4.testAll
        , testCase "Tut5"       Tut5.testAll
        , testCase "Tut6"       Tut6.testAll
        , testCase "Tut7"       Tut7.testAll
        , testCase "Concurrent" Concurrent.testAll
        , testProperty "fft_dft"  $ prop_fft_dft dft' fft'
        , testProperty "dft_idft" $ prop_inverse dft' idft'
        , testProperty "fft_ifft" $ prop_inverse fft' ifft'
        ]

  where
    marshalledM = marshalled' def def {externalFlagsPost = ["-lm"]}

