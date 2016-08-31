{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Prelude

import Data.List (genericIndex, genericLength, mapAccumL)

import qualified Test.QuickCheck.Monadic as QC

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Feldspar.Run
import Feldspar.Data.Array
import Feldspar.Data.Queue



--------------------------------------------------------------------------------
-- * Marshalling
--------------------------------------------------------------------------------

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



--------------------------------------------------------------------------------
-- * Nested indexing
--------------------------------------------------------------------------------

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



--------------------------------------------------------------------------------
-- * Queues
--------------------------------------------------------------------------------

-- | Interpreter for queue operations
--
-- Instructions are read as a list of triples:
--
-- * @(0,i,_)@ means get the element at index @i@ and print it to @stdout@
-- * @(1,_,p)@ put @p@ into the queue
interpQueue :: (Syntax a, MarshalFeld a) => Queue a -> Run ()
interpQueue q = streamStdIO $ \(cmd :: Data Word8, ip, pp) -> do
    r <- newRef
    iff (cmd==0)
      (indexQ q ip >>= setRef r)
      (return ())
    iff (cmd==1)
      (putQ q pp >> indexQ q 0 >>= setRef r) $
      (return ())
    getRef r

-- | Make a queue interpreter using 'newQueue'
interpQ1
    :: Data Length  -- ^ Queue size
    -> Run ()
interpQ1 s = do
    q :: Queue (Data Index) <- newQueue s
    interpQueue q

-- | Make a queue interpreter using 'newQueue2'
interpQ2
    :: Data Length  -- ^ Queue size
    -> Run ()
interpQ2 s = do
    q :: Queue (Data Index) <- newQueue2 s
    interpQueue q

myMarshalled :: (MarshalHaskell a, MarshalHaskell b, MonadRun m)
    => m ()
    -> ((a -> IO b) -> IO c)
    -> IO c
myMarshalled prog body =
    withCompiled prog $ \f ->
      body (fmap (parse toHaskell) . f . fromHaskell)

-- | @(command, index parameter, put parameter)@
type QCMD a = (Word8, Index, a)

genQCMD :: Arbitrary a
    => Length  -- ^ Queue size
    -> Gen [QCMD a]
genQCMD len = do
    NonNegative (n :: Int) <- arbitrary
    Prelude.reverse <$> go n 0 []
  where
    go 0 s prog = return prog
    go n 0 prog = do
      a <- arbitrary
      go (n-1) 1 ((1,0,a):prog)
    go n s prog = do
      c <- choose (0,1)
      i <- choose (0,s-1)
      a <- arbitrary
      let c' = Prelude.fromIntegral c
      go (n-1) (Prelude.min len (s+c')) ((c,i,a):prog)

-- | Reference implementation of a queue interpreter
interpQ_ref :: [QCMD a] -> [a]
interpQ_ref = snd . mapAccumL interp []
  where
    interp q (0,i,_) = (q, genericIndex q i)
    interp q (1,_,a) = (a:q, a)

-- | Test that two queue implementations give the same result
property_queue len qprog1 qprog2 = QC.monadicIO $ do
    cmd <- QC.pick (genQCMD len)
    os1 <- QC.run $ qprog1 cmd
    os2 <- QC.run $ qprog2 cmd
    QC.assert (os1 Prelude.== os2)



--------------------------------------------------------------------------------
-- * Main
--------------------------------------------------------------------------------

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

    myMarshalled (interpQ1 5) $ \(iq1 :: [QCMD Index] -> IO [Index]) ->
    myMarshalled (interpQ2 5) $ \iq2 ->

      defaultMain $ testGroup "tests"
        [ $testGroupGenerator
        , testGroup "marshalling"
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
        , testGroup "nested indexing"
            [ testProperty "nestIndex"       $ property_nestIndex nestIndex_c
            , testProperty "nestIndexLength" $ property_nestIndexLength nestIndexLength_c
            ]
        , testGroup "queues"
            [ testProperty "queue ref"     $ property_queue 5 (return . interpQ_ref) iq2
            , testProperty "queue 1 vs. 2" $ property_queue 5 iq1 iq2
            ]
        ]

