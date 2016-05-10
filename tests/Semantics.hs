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

check_marshalFeld :: forall proxy a
    .  ( MarshalFeld a
       , Eq (HaskellRep a)
       , Show (HaskellRep a)
       , Arbitrary (HaskellRep a)
       )
    => proxy a  -- Dummy argument, just to force the type
    -> IO ()
check_marshalFeld a = marshalled (return :: a -> Run a) $
    QC.quickCheck . generic_prop_marshalFeld

main = do
    check_marshalFeld (Nothing :: Maybe (Data Int32))
    check_marshalFeld (Nothing :: Maybe (Data Word32))
    check_marshalFeld (Nothing :: Maybe (Data Double))
    check_marshalFeld (Nothing :: Maybe (Data (Complex Float)))
    check_marshalFeld (Nothing :: Maybe (WithLength (Arr Int32)))
    check_marshalFeld (Nothing :: Maybe (WithLength (IArr Int32)))
    check_marshalFeld (Nothing :: Maybe ((Data Int32, Data Int32)))
    check_marshalFeld (Nothing :: Maybe ((WithLength (IArr Int32), (Data Int32, WithLength (Arr Int32)))))
    $defaultMainGenerator

