{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Prelude

import Data.List (genericIndex, genericLength)

import qualified Test.QuickCheck.Monadic as QC

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Feldspar.Run
import Feldspar.Data.Array



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

-- | Indexing in a 3-dimensional structure
nestIndex
    :: ( DIArr Int32
       , (Data Length, Data Length, Data Length)
       , (Data Index,  Data Index,  Data Index)
       )
    -> Run (Data Int32)
nestIndex (arr,(l1,l2,l3),(i1,i2,i3)) = return $ nestedArr ! i1 ! i2 ! i3
  where
    nestedArr = multiNest (Outer :> l2 :> l3) arr

property_nestIndex f =
    forAll genLenIx $ \(l1,i1) ->
    forAll genLenIx $ \(l2,i2) ->
    forAll genLenIx $ \(l3,i3) ->
      let l = Prelude.fromIntegral (l1*l2*l3)
      in  forAll (vector l) $ \(as :: [Int32]) -> QC.monadicIO $ do
            a <- QC.run $ f (as,(l1,l2,l3),(i1,i2,i3))
            QC.assert (a Prelude.== genericIndex as (i1*l2*l3 + i2*l3 + i3))
  where
    genLenIx = do
        l <- choose (1,5)
        i <- choose (0,l-1)
        return (l,i)

-- | Indexing in a 3-dimensional structure
nestIndexLength
    :: ( DIArr Int32
       , (Data Length, Data Length, Data Length)
       , (Data Index, Data Index)
       )
    -> Run (DIArr Int32)
nestIndexLength (arr,(l1,l2,l3),(i1,i2)) = return $ nestedArr ! i1 ! i2
  where
    nestedArr = multiNest (Outer :> l2 :> l3) arr

property_nestIndexLength f =
    forAll genLenIx $ \(l1,i1) ->
    forAll genLenIx $ \(l2,i2) ->
    forAll (choose (1,5)) $ \l3 ->
      let l = Prelude.fromIntegral (l1*l2*l3)
      in  forAll (vector l) $ \(as :: [Int32]) -> QC.monadicIO $ do
            bs <- QC.run $ f (as,(l1,l2,l3),(i1,i2))
            QC.assert (genericLength bs Prelude.== l3)
  where
    genLenIx = do
        l <- choose (1,5)
        i <- choose (0,l-1)
        return (l,i)

main =
    marshalled (return :: _ -> Run (Data Int8))               $ \f_Int8 ->
    marshalled (return :: _ -> Run (Data Int16))              $ \f_Int16 ->
    marshalled (return :: _ -> Run (Data Int32))              $ \f_Int32 ->
    marshalled (return :: _ -> Run (Data Int64))              $ \f_Int64 ->
    marshalled (return :: _ -> Run (Data Word8))              $ \f_Word8 ->
    marshalled (return :: _ -> Run (Data Word16))             $ \f_Word16 ->
    marshalled (return :: _ -> Run (Data Word32))             $ \f_Word32 ->
    marshalled (return :: _ -> Run (Data Word64))             $ \f_Word64 ->
    marshalled (return :: _ -> Run (Data Float))              $ \f_Float  ->
    marshalled (return :: _ -> Run (Data Double))             $ \f_Double ->
    marshalled (return :: _ -> Run (Data (Complex Float)))    $ \f_CompFloat ->
    marshalled (return :: _ -> Run (Data (Complex Double)))   $ \f_CompDouble ->
    marshalled (return :: _ -> Run (DArr Double))             $ \f_Arr ->
    marshalled (return :: _ -> Run (DIArr Int32))             $ \f_IArr ->
    marshalled (return :: _ -> Run (Data Word8, Data Double)) $ \f_Pair ->
    marshalled (return :: _ -> Run (DIArr Double, (Data Int8, DArr (Complex Float)))) $ \f_Nested ->

    marshalled nestIndex       $ \nestIndex_c ->
    marshalled nestIndexLength $ \nestIndexLength_c ->

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
        , testGroup "misc"
            [ testProperty "nestIndex"       $ property_nestIndex nestIndex_c
            , testProperty "nestIndexLength" $ property_nestIndexLength nestIndexLength_c
            ]
        ]

