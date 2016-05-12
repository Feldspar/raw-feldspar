{-# LANGUAGE TemplateHaskell #-}

import qualified Test.QuickCheck.Monadic as QC

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Feldspar.Run



generic_prop_marshalHaskell a = parse toHaskell (fromHaskell a) Prelude.== a

prop_marshalHaskell_Int8       a = generic_prop_marshalHaskell (a :: Int8)
prop_marshalHaskell_Int16      a = generic_prop_marshalHaskell (a :: Int16)
prop_marshalHaskell_Int32      a = generic_prop_marshalHaskell (a :: Int32)
prop_marshalHaskell_Int64      a = generic_prop_marshalHaskell (a :: Int64)
prop_marshalHaskell_Word8      a = generic_prop_marshalHaskell (a :: Word8)
prop_marshalHaskell_Word16     a = generic_prop_marshalHaskell (a :: Word16)
prop_marshalHaskell_Word32     a = generic_prop_marshalHaskell (a :: Word32)
prop_marshalHaskell_Word64     a = generic_prop_marshalHaskell (a :: Word64)
prop_marshalHaskell_Float      a = generic_prop_marshalHaskell (a :: Float)
prop_marshalHaskell_Double     a = generic_prop_marshalHaskell (a :: Double)
prop_marshalHaskell_CompFloat  a = generic_prop_marshalHaskell (a :: Complex Float)
prop_marshalHaskell_CompDouble a = generic_prop_marshalHaskell (a :: Complex Double)
prop_marshalHaskell_List       a = generic_prop_marshalHaskell (a :: [Double])
prop_marshalHaskell_Pair       a = generic_prop_marshalHaskell (a :: (Word8,Double))
prop_marshalHaskell_Nested     a = generic_prop_marshalHaskell (a :: ([Double],(Int8,[Complex Float])))
prop_marshalHaskell_ListOfList a = generic_prop_marshalHaskell (a :: [(Int8,[Word8])])

property_marshalFeld f a = QC.monadicIO $ do
    b <- QC.run $ f a
    QC.assert (a Prelude.== b)

type Pass a = a -> Run a

main =
    marshalled (return :: Pass (Data Int8))             $ \f_Int8 ->
    marshalled (return :: Pass (Data Int16))            $ \f_Int16 ->
    marshalled (return :: Pass (Data Int32))            $ \f_Int32 ->
    marshalled (return :: Pass (Data Int64))            $ \f_Int64 ->
    marshalled (return :: Pass (Data Word8))            $ \f_Word8 ->
    marshalled (return :: Pass (Data Word16))           $ \f_Word16 ->
    marshalled (return :: Pass (Data Word32))           $ \f_Word32 ->
    marshalled (return :: Pass (Data Word64))           $ \f_Word64 ->
    marshalled (return :: Pass (Data Float))            $ \f_Float  ->
    marshalled (return :: Pass (Data Double))           $ \f_Double ->
    marshalled (return :: Pass (Data (Complex Float)))  $ \f_CompFloat ->
    marshalled (return :: Pass (Data (Complex Double))) $ \f_CompDouble ->
    marshalled (return :: Pass (Dim (Arr Double)))      $ \f_Arr ->
    marshalled (return :: Pass (Dim (IArr Int32)))      $ \f_IArr ->
    marshalled (return :: Pass (Data Word8, Data Double)) $ \f_Pair ->
    marshalled (return :: Pass (Dim (IArr Double), (Data Int8, Dim (Arr (Complex Float))))) $ \f_Nested ->

      defaultMain $ testGroup "tests"
        [ $testGroupGenerator
        , testGroup "marshalFeld"
            [ testProperty "marshalFeld Int8"           $ property_marshalFeld f_Int8
            , testProperty "marshalFeld Int16"          $ property_marshalFeld f_Int16
            , testProperty "marshalFeld Int32"          $ property_marshalFeld f_Int32
            , testProperty "marshalFeld Int64"          $ property_marshalFeld f_Int64
            , testProperty "marshalFeld Word8"          $ property_marshalFeld f_Word8
            , testProperty "marshalFeld Word16"         $ property_marshalFeld f_Word16
            , testProperty "marshalFeld Word32"         $ property_marshalFeld f_Word32
            , testProperty "marshalFeld Word64"         $ property_marshalFeld f_Word64
            , testProperty "marshalFeld Float"          $ property_marshalFeld f_Float
            , testProperty "marshalFeld Double"         $ property_marshalFeld f_Double
            , testProperty "marshalFeld Complex Float"  $ property_marshalFeld f_CompFloat
            , testProperty "marshalFeld Complex Double" $ property_marshalFeld f_CompDouble
            , testProperty "marshalFeld Arr"            $ property_marshalFeld f_Arr
            , testProperty "marshalFeld IArr"           $ property_marshalFeld f_IArr
            , testProperty "marshalFeld Pair"           $ property_marshalFeld f_Pair
            , testProperty "marshalFeld Nested"         $ property_marshalFeld f_Nested
            ]
        ]

