{-# LANGUAGE TemplateHaskell #-}

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Feldspar.Run



generic_prop_marshalHaskell a = parse toHaskell (fromHaskell a) Prelude.== a

prop_marshalHaskell_Int8       a = generic_prop_marshalHaskell (a :: Int8)
prop_marshalHaskell_Word8      a = generic_prop_marshalHaskell (a :: Word8)
prop_marshalHaskell_Double     a = generic_prop_marshalHaskell (a :: Double)
prop_marshalHaskell_CompFloat  a = generic_prop_marshalHaskell (a :: Complex Float)
prop_marshalHaskell_List       a = generic_prop_marshalHaskell (a :: [Double])
prop_marshalHaskell_Pair       a = generic_prop_marshalHaskell (a :: (Word8,Double))
prop_marshalHaskell_Nested     a = generic_prop_marshalHaskell (a :: ([Double],(Int8,[Complex Float])))
prop_marshalHaskell_ListOfList a = generic_prop_marshalHaskell (a :: [(Int8,[Word8])])

generic_prop_marshalFeld f a = QC.monadicIO $ do
    b <- QC.run $ f a
    QC.assert (a Prelude.== b)

check_marshalFeld_Int32 = marshalled (return :: Data Int32 -> Run (Data Int32)) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_Word32 = marshalled (return :: Data Word32 -> Run (Data Word32)) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_Double = marshalled (return :: Data Double -> Run (Data Double)) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_CompFloat = marshalled (return :: Data (Complex Float) -> Run (Data (Complex Float))) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_Arr = marshalled (return :: WithLength (Arr Int32) -> Run (WithLength (Arr Int32))) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_IArr = marshalled (return :: WithLength (IArr Int32) -> Run (WithLength (IArr Int32))) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_Pair = marshalled (return :: (Data Int32, Data Int32) -> Run (Data Int32, Data Int32)) $
    QC.quickCheck . generic_prop_marshalFeld

check_marshalFeld_Nested = marshalled (return :: (WithLength (IArr Int32), (Data Int32, WithLength (Arr Int32))) -> Run (WithLength (IArr Int32), (Data Int32, WithLength (Arr Int32)))) $
    QC.quickCheck . generic_prop_marshalFeld

main = do
    check_marshalFeld_Int32
    check_marshalFeld_Word32
--     check_marshalFeld_Double
--     check_marshalFeld_CompFloat
    check_marshalFeld_Arr
    check_marshalFeld_IArr
    check_marshalFeld_Pair
    check_marshalFeld_Nested
    $defaultMainGenerator

