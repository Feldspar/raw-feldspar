{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module LDPC where

import Feldspar
import Feldspar.Run
import Feldspar.Hardware
import qualified Feldspar.Hardware.Compile as HW
import qualified Feldspar.Run.Compile      as SW

import Feldspar.Representation
import Feldspar.Primitive.Representation (Primitive)

import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Language.Embedded.Hardware.Expression.Represent.Bit
import Language.Embedded.Imperative (FreeExp)
import Language.Embedded.Expression (FreePred)

import Data.Word
import Data.Int

import Data.TypedStruct

import Data.Typeable
import GHC.TypeLits

import qualified Prelude as P

--------------------------------------------------------------------------------
-- * LDPC Software.
--------------------------------------------------------------------------------

data Matrix exp a = Matrix (exp Length) (exp Length) (Arr exp (Internal a))

type Mat exp a = Matrix exp (exp a)

--------------------------------------------------------------------------------

getMat
  :: ( MonadComp exp m
     , Syntax exp a
     , NUM exp
     , PrimTypeOf exp Index
     , FreeExp exp
     , FreeDict exp)
  => exp Index -> exp Index -> Matrix exp a -> m a
getMat i j (Matrix rows cols array) = getArr ((rows `times` i) `plus` j) array

setMat
  :: ( MonadComp exp m
     , Syntax exp a
     , NUM exp
     , PrimTypeOf exp Index
     , FreeExp exp
     , FreeDict exp
     )
  => exp Index -> exp Index -> a -> Matrix exp a -> m ()
setMat i j v (Matrix rows cols array) = setArr ((rows `times` i) `plus` j) v array

----------------------------------------

getCol
  :: forall m exp a.
     ( MonadComp exp m
     , Syntax exp (exp a)
     , NUM exp
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp a) ~ a)
  => exp Index -> Mat exp a -> m (Arr exp a)
getCol col m@(Matrix rows cols array) = do
  v <- newArr rows
  for rows $ \i -> do
    x :: exp a <- getMat i col m
    setArr i x v
  return v

getRow
  :: forall m exp a.
     ( MonadComp exp m
     , Syntax exp (exp a)
     , NUM exp
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp a) ~ a)
  => exp Index -> Mat exp a -> m (Arr exp a)
getRow row m@(Matrix rows cols array) = do
  v <- newArr cols
  for cols $ \j -> do
    x :: exp a <- getMat row j m
    setArr j x v
  return v

--------------------------------------------------------------------------------

vecmul
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Bit)
     , BOOL exp
     , NUM exp
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Bit) ~ Bit
     ) 
  => Mat exp Bit     -- M x N
  -> Arr exp Bit     -- N x 1
  -> m (Arr exp Bit) -- M x 1
vecmul h@(Matrix rows cols array) vec = do
  v <- newArr rows
  for rows $ \i ->
    setArr i (false :: exp Bit) v
  for cols $ \j -> do
    b <- getArr j vec
    when b $
      for rows $ \i -> do
        x <- getArr i v
        y <- getMat i j h
        setArr i ((not x && y) || (x && not y)) v
  return v

--------------------------------------------------------------------------------

check
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Bit)
     , BOOL exp
     , NUM exp
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Bit) ~ Bit
     )
  => Mat exp Bit -> Arr exp Bit -> m (exp Bool)
check h@(Matrix rows cols array) vec = do
  v  <- vecmul h vec
  ok <- initRef (true :: exp Bit)
  for rows $ \i -> do
    b <- getArr i v
    when b $
      setRef ok (false :: exp Bit)
  getRef ok

--------------------------------------------------------------------------------

init
  :: forall exp m.
     ( MonadComp exp m
     , Syntax exp (exp Bit)
     , Syntax exp (exp Float)
     , VAL exp
     , NUM exp
     , ORD exp
     , PrimTypeOf exp Float
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Bit) ~ Bit
     , Internal (exp Float) ~ Float)
  => Mat exp Bit        -- parity-check matrix
  -> Arr exp Float      -- likelihood ratios for bits
  -> m ( Arr exp Bit    -- decoded word
       , Mat exp Float  -- likelihoods
       , Mat exp Float) -- probabilities
init h@(Matrix rows cols _) vec = do
  lra :: Arr exp Float <- newArr (rows `times` cols)
  pra :: Arr exp Float <- newArr (rows `times` cols)
  dec :: Arr exp Bit   <- newArr cols
  let lr = Matrix rows cols lra :: Mat exp Float
      pr = Matrix rows cols pra :: Mat exp Float
  for cols $ \j -> do
    v :: exp Float <- getArr j vec
    for rows $ \i -> do
      b <- getMat i j h
      when b $ do
        setMat i j v pr
        setMat i j (value 1 :: exp Float) lr
    setArr j (v `gte` (value 1 :: exp Float)) dec
  return (dec, lr, pr)

--------------------------------------------------------------------------------

iter 
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Bit)
     , Syntax exp (exp Float)
     , VAL exp
     , NUM exp
     , FRAC exp
     , ORD exp
     , PrimTypeOf exp Float
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Bit) ~ Bit
     , Internal (exp Float) ~ Float)
  => Mat exp Bit   -- parity-check matrix.
  -> Mat exp Float -- likelihoods.
  -> Mat exp Float -- probabilities.
  -> Arr exp Float -- likelihood ratios for codeword.
  -> Arr exp Bit   -- decoded word.
  -> m ()
iter h@(Matrix rows cols _) lr pr vec dec = do
  dl :: Ref exp Float <- newRef
  dr :: Ref exp Float <- newRef

  -- Recompute lr.
  for rows $ \j -> do
    setRef dl (value 1 :: exp Float)
    for cols $ \i -> do
      b <- getMat i j h
      when b $ do
        v <- getRef dl
        x <- getMat i j pr
        setMat i j v lr
        setRef dl (v `times` (value 2 `divide` ((value 1 `plus` x) `minus` value 1)))
    setRef dl (value 1 :: exp Float)
    for cols $ \i -> do
      let i' = rows `minus` i
      b <- getMat i' j h
      when b $ do
        v <- getRef dl
        x <- getMat i' j lr
        y <- getMat i' j pr
        let t = x `times` v
        setMat i' j ((value 1 `minus` t) `divide` (value 1 `plus` t)) lr
        setRef dl (v `times` (value 2 `divide` ((value 1 `plus` y) `minus` value 1)))

  -- Recompute pr.
  for cols $ \j -> do
    t :: exp Float <- getArr j vec
    setRef dr t
    for rows $ \i -> do
      b <- getMat i j h
      when b $ do
        v <- getRef dr
        x <- getMat i j lr
        setMat i j v pr
        setRef dr (v `times` x)
    p :: exp Float <- getRef dr
    setArr j (p `gte` value 1) dec
    setRef dr (value 1 :: exp Float)
    for rows $ \i -> do
      let i' = rows `minus` i
      b <- getMat i' j h
      when b $ do
        v <- getRef dr
        x <- getMat i' j pr
        y <- getMat i' j lr
        setMat i' j (x `times` v) pr
        setRef dr (v `times` y)

--------------------------------------------------------------------------------
-- Software decode.

decode
  :: forall m. MonadComp Data m
  => Mat Data Bit
  -> Arr Data Float
  -> m (Data Bool)
decode h vec = do
  done <- initRef (false :: Data Bool)
  (dec, lr, pr) <- init h vec
  for (value 10 :: Data Word32) $ \_ -> do
    b <- check h dec
    when b $ do
      setRef done (true :: Data Bool)
      break
    iter h lr pr vec dec
  getRef done

--------------------------------------------------------------------------------
-- * LDPC Hardware
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- or andy's version on check
cardinatlity
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Bit)
     , Syntax exp (exp Float)
     , VAL exp
     , NUM exp
     , ORD exp
     , BOOL exp
     , PrimTypeOf exp Float
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Bit) ~ Bit
     , Internal (exp Float) ~ Float)
  => Mat exp Bit
  -> Arr exp Float
  -> m (exp Bool)
cardinatlity h@(Matrix rows cols array) vec = do
    c  <- c_hat cols vec
    a  <- ans h c
    ok <- initRef (true :: exp Bool)
    for rows $ \i -> do
      b <- getArr i a
      when b $
        setRef ok (false :: exp Bool)
    getRef ok
  where
    c_hat :: exp Length -> Arr exp Float -> m (Arr exp Bit)
    c_hat len arr = do
      v <- newArr len
      for len $ \i -> do
        c <- getArr i arr
        setArr i (c `gte` (value 0 :: exp Float)) v
      return v

    ans :: Mat exp Bit -> Arr exp Bit -> m (Arr exp Bit)
    ans h@(Matrix rows cols array) vec = do
      v <- newArr rows
      for rows $ \i -> do
        setArr i (false :: exp Bit) v
      for cols $ \j -> do
        b <- getArr j vec
        when b $
          for rows $ \i -> do
            x <- getArr i v
            y <- getMat i j h
            setArr i ((not x && y) || (x && not y)) v
      return v

--------------------------------------------------------------------------------

ne'
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Float)
     , Syntax exp (exp Bit)
     , VAL exp
     , NUM exp
     , EQ exp
     , BOOL exp
     , PrimTypeOf exp Float
     , PrimTypeOf exp Length
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , Internal (exp Float) ~ Float)
  => Mat exp Bit   -- parity-check matrix.
  -> exp Index     -- current n
  -> Mat exp Float -- ne
  -> Arr exp Float -- lam
  -> m (Mat exp Float)
ne' h@(Matrix rows cols _) n ne lam = do
  nea :: Arr exp Float <- newArr (rows `times` cols)
  vr  :: Ref exp Float <- newRef
  let ne' = (Matrix rows cols nea)
  for cols $ \j ->
    for rows $ \i -> do
      setRef vr (value 1 :: exp Float)
      b <- getMat i j h
      when b $ do        
        for cols $ \j' -> 
          when (n `neq` j') $ do
            l :: exp Float <- getArr j' lam
            n :: exp Float <- getMat i j' ne
            modifyRef vr (metric (n `minus` l))
        v :: exp Float <- getRef vr
        setMat i j ((value 0.75 :: exp Float) `times` v) ne'
  return ne'

metric :: exp Float -> exp Float -> exp Float
metric x y = x

--------------------------------------------------------------------------------

lam'
  :: forall m exp.
     ( MonadComp exp m
     , Syntax exp (exp Float)
     , Syntax exp (exp Bit)
     , VAL exp
     , NUM exp
     , TypeOf exp Float
     , PrimTypeOf exp Float
     , FreeDict exp
     , FreeExp exp
     , FreePred exp Length
     , FreePred exp ~ PredOf exp
     , PrimTypeOf exp Length
     , Internal (exp Float) ~ Float)
  => Mat exp Bit       -- parity-check matrix
  -> Mat exp Float     -- ne'
  -> Arr exp Float     -- orig_lam
  -> m (Arr exp Float) -- lam'
lam' h@(Matrix rows cols _) ne' orig = do
  l'  :: Arr exp Float <- newArr cols
  sum :: Ref exp Float <- newRef
  for cols $ \j -> do
    setRef sum (value 0 :: exp Float)
    for rows $ \i -> do
      b <- getMat i j h
      when b $ do
        v :: exp Float <- getMat i j ne'
        modifyRef sum (plus v)
    old :: exp Float <- getArr j orig
    v   :: exp Float <- getRef sum
    setArr j (old `plus` v) l'
  return l'

--------------------------------------------------------------------------------
-- * Tests
--------------------------------------------------------------------------------
{-
testCheck = SW.icompile prog
  where
    prog :: Run ()
    prog = do
      r :: Ref Data Length <- initRef (value 3 :: Data Length)
      c :: Ref Data Length <- initRef (value 6 :: Data Length)
      a :: Arr Data Bit    <- initArr [
          one,  one, zero, one, zero, zero,
          zero, one, one,  zero, one,  zero,
          one,  one, one,  zero, zero, one
        ]
      h :: Mat Data Bit <- readStoreRep (r, c, a)
      y :: Arr Data Bit <- initArr [
          zero, one, zero, one,  zero, zero
        ]
      b :: Data Bool <- check h y
      printf "OK? %d" (b2i b :: Data Word32)

testInit = SW.icompile prog
  where
    prog :: Run ()
    prog = do
      r :: Ref Data Length <- initRef (value 3 :: Data Length)
      c :: Ref Data Length <- initRef (value 6 :: Data Length)
      a :: Arr Data Bit    <- initArr [
          one,  one, zero, one, zero, zero,
          zero, one, one,  zero, one,  zero,
          one,  one, one,  zero, zero, one
        ]
      h :: Mat Data Bit   <- readStoreRep (r, c, a)
      y :: Arr Data Float <- initArr [
          0, 5.2, 0, 4.2, -5.2, -5.4
        ]
      init h y
      return ()

testIter = SW.icompile prog
  where
    prog :: Run ()
    prog = do
      r :: Ref Data Length <- initRef (value 3 :: Data Length)
      c :: Ref Data Length <- initRef (value 6 :: Data Length)
      a :: Arr Data Bit    <- initArr [
          one,  one, zero, one, zero, zero,
          zero, one, one,  zero, one,  zero,
          one,  one, one,  zero, zero, one
        ]
      h :: Mat Data Bit   <- readStoreRep (r, c, a)
      y :: Arr Data Float <- initArr [
          0, 5.2, 0, 4.2, -5.2, -5.4
        ]
      (dec, lr, pr) <- init h y
      iter h lr pr y dec

one  :: Bit
one  = True

zero :: Bit
zero = False
-}

--------------------------------------------------------------------------------
-- * Old
--------------------------------------------------------------------------------
{-
data Matrix exp a = Matrix (exp Length) (exp Length) (exp Index -> exp Index -> a)

type instance ExprOf (Matrix exp a) = exp

instance ( Syntax exp a
         , Syntax exp (exp Length)
         , NUM exp
         , ArrIx exp
         , FreeExp exp
         , FreeDict exp
         , PrimTypeOf exp Length
         , Internal (exp Length) ~ Length
         , FreePred exp  ~ PredOf exp
         , FreePred exp Length
         )
    => Storable (Matrix exp a)
  where
    type StoreRep  (Matrix exp a) = (Ref exp Length, Ref exp Length, Arr exp (Internal a))

    type StoreSize (Matrix exp a) = (exp Length, exp Length)

    newStoreRep _ (rows, cols) = do
      arr :: Arr exp (Internal a) <- newArr (rows `times` cols)
      r   :: Ref exp Length <- initRef rows
      c   :: Ref exp Length <- initRef cols
      return (r, c, arr)
    initStoreRep m@(Matrix rows cols _) = do
      arr :: Arr exp (Internal a) <- newArr (rows `times` cols)
      r   :: Ref exp Length <- initRef rows
      c   :: Ref exp Length <- initRef cols
      let n = (r, c, arr)
      writeStoreRep n m
      return n
    readStoreRep (rows, cols, array) = do
      r <- getRef rows
      c <- getRef cols
      a <- freezeArr array (r `times` c)
      return (Matrix r c (\i j -> a `arrIx` ((i `times` c) `plus` j)))
    writeStoreRep (rows, cols, array) (Matrix r c ixf) = do
      offset :: exp Length <- unsafeFreezeRef cols
      for r $ \i ->
        for c $ \j ->
          setArr ((i `times` offset) `plus` j) (ixf i j) array
    unsafeFreezeStoreRep = P.error "storable-matrix: unsafeFreezeStoreRep"
    copyStoreRep         = P.error "storable-matrix: copyStoreRep"
-}
--------------------------------------------------------------------------------
-- ** Psi lookup table.
--
-- ^ From 'FPGA implementation of a Flexible LDPC decoder.

lut_signature :: Signature (
     Signal (Bits 8)
  -> Signal (Bits 7)
  -> ())
lut_signature =
  input  $ \inp ->
  output $ \out ->
  ret $ lut inp out

lut :: Signal (Bits 8) -> Signal (Bits 7) -> Hardware ()
lut inp out = process (inp .: []) $
  do v <- asSigned inp
     switch v
       [ is (-128) $ out <== value 0 
       , is (-127) $ out <== value 0 
       , is (-126) $ out <== value 0 
       , is (-125) $ out <== value 0 
       , is (-124) $ out <== value 0 
       , is (-123) $ out <== value 0 
       , is (-122) $ out <== value 0 
       , is (-121) $ out <== value 0 
       , is (-120) $ out <== value 0 
       , is (-119) $ out <== value 0 
       , is (-118) $ out <== value 0 
       , is (-117) $ out <== value 0 
       , is (-116) $ out <== value 0 
       , is (-115) $ out <== value 0 
       , is (-114) $ out <== value 0 
       , is (-113) $ out <== value 0 
       , is (-112) $ out <== value 0 
       , is (-111) $ out <== value 0 
       , is (-110) $ out <== value 0 
       , is (-109) $ out <== value 0 
       , is (-108) $ out <== value 0 
       , is (-107) $ out <== value 0 
       , is (-106) $ out <== value 0 
       , is (-105) $ out <== value 0 
       , is (-104) $ out <== value 0 
       , is (-103) $ out <== value 0 
       , is (-102) $ out <== value 0 
       , is (-101) $ out <== value 0 
       , is (-100) $ out <== value 0 
       , is (-99) $ out <== value 0  
       , is (-98) $ out <== value 0  
       , is (-97) $ out <== value 1
       , is (-96) $ out <== value 1
       , is (-95) $ out <== value 1
       , is (-94) $ out <== value 1
       , is (-93) $ out <== value 1
       , is (-92) $ out <== value 1
       , is (-91) $ out <== value 1
       , is (-90) $ out <== value 1
       , is (-89) $ out <== value 1
       , is (-88) $ out <== value 1
       , is (-87) $ out <== value 1
       , is (-86) $ out <== value 1
       , is (-85) $ out <== value 1
       , is (-84) $ out <== value 1
       , is (-83) $ out <== value 1
       , is (-82) $ out <== value 1
       , is (-81) $ out <== value 1
       , is (-80) $ out <== value 1
       , is (-79) $ out <== value 1
       , is (-78) $ out <== value 1
       , is (-77) $ out <== value 1
       , is (-76) $ out <== value 1
       , is (-75) $ out <== value 1
       , is (-74) $ out <== value 1
       , is (-73) $ out <== value 1
       , is (-72) $ out <== value 1
       , is (-71) $ out <== value 1
       , is (-70) $ out <== value 1
       , is (-69) $ out <== value 1
       , is (-68) $ out <== value 1  
       , is (-67) $ out <== value 1  
       , is (-66) $ out <== value 1  
       , is (-65) $ out <== value 1  
       , is (-64) $ out <== value 1  
       , is (-63) $ out <== value 1  
       , is (-62) $ out <== value 1  
       , is (-61) $ out <== value 1  
       , is (-60) $ out <== value 1
       , is (-59) $ out <== value 1
       , is (-58) $ out <== value 1
       , is (-57) $ out <== value 1
       , is (-56) $ out <== value 2
       , is (-55) $ out <== value 2
       , is (-54) $ out <== value 2
       , is (-53) $ out <== value 2
       , is (-52) $ out <== value 2
       , is (-51) $ out <== value 2
       , is (-50) $ out <== value 2
       , is (-49) $ out <== value 2
       , is (-48) $ out <== value 2
       , is (-47) $ out <== value 2
       , is (-46) $ out <== value 2
       , is (-45) $ out <== value 2
       , is (-44) $ out <== value 3
       , is (-43) $ out <== value 3
       , is (-42) $ out <== value 3
       , is (-41) $ out <== value 3
       , is (-40) $ out <== value 3
       , is (-39) $ out <== value 3
       , is (-38) $ out <== value 3
       , is (-37) $ out <== value 3
       , is (-36) $ out <== value 4
       , is (-35) $ out <== value 4
       , is (-34) $ out <== value 4
       , is (-33) $ out <== value 4
       , is (-32) $ out <== value 4
       , is (-31) $ out <== value 5
       , is (-30) $ out <== value 5
       , is (-29) $ out <== value 6
       , is (-28) $ out <== value 6
       , is (-27) $ out <== value 7
       , is (-26) $ out <== value 7
       , is (-25) $ out <== value 8
       , is (-24) $ out <== value 8
       , is (-23) $ out <== value 9
       , is (-22) $ out <== value 9
       , is (-21) $ out <== value 10
       , is (-20) $ out <== value 10
       , is (-19) $ out <== value 11
       , is (-18) $ out <== value 11
       , is (-17) $ out <== value 12
       , is (-16) $ out <== value 12
       , is (-15) $ out <== value 13
       , is (-14) $ out <== value 14
       , is (-13) $ out <== value 15
       , is (-12) $ out <== value 16
       , is (-11) $ out <== value 18
       , is (-10) $ out <== value 20
       , is (-9) $ out <== value 22
       , is (-8) $ out <== value 24
       , is (-7) $ out <== value 26
       , is (-6) $ out <== value 28
       , is (-5) $ out <== value 30
       , is (-4) $ out <== value 32
       , is (-3) $ out <== value 40
       , is (-2) $ out <== value 47
       , is (-1) $ out <== value 79
       , is (0) $ out <== value 127
       , is (127) $ out <== value 0
       , is (126) $ out <== value 0
       , is (125) $ out <== value 0
       , is (124) $ out <== value 0
       , is (123) $ out <== value 0
       , is (122) $ out <== value 0
       , is (121) $ out <== value 0
       , is (120) $ out <== value 0
       , is (119) $ out <== value 0
       , is (118) $ out <== value 0
       , is (117) $ out <== value 0
       , is (116) $ out <== value 0
       , is (115) $ out <== value 0
       , is (114) $ out <== value 0
       , is (113) $ out <== value 0
       , is (112) $ out <== value 0
       , is (111) $ out <== value 0
       , is (110) $ out <== value 0
       , is (109) $ out <== value 0
       , is (108) $ out <== value 0
       , is (107) $ out <== value 0
       , is (106) $ out <== value 0
       , is (105) $ out <== value 0
       , is (104) $ out <== value 0
       , is (103) $ out <== value 0
       , is (102) $ out <== value 0
       , is (101) $ out <== value 0
       , is (100) $ out <== value 0
       , is (99) $ out <== value 0
       , is (98) $ out <== value 0
       , is (97) $ out <== value 1
       , is (96) $ out <== value 1
       , is (95) $ out <== value 1
       , is (94) $ out <== value 1
       , is (93) $ out <== value 1
       , is (92) $ out <== value 1
       , is (91) $ out <== value 1
       , is (90) $ out <== value 1
       , is (89) $ out <== value 1
       , is (88) $ out <== value 1
       , is (87) $ out <== value 1
       , is (86) $ out <== value 1
       , is (85) $ out <== value 1
       , is (84) $ out <== value 1
       , is (83) $ out <== value 1
       , is (82) $ out <== value 1
       , is (81) $ out <== value 1
       , is (80) $ out <== value 1
       , is (79) $ out <== value 1
       , is (78) $ out <== value 1
       , is (77) $ out <== value 1
       , is (76) $ out <== value 1
       , is (75) $ out <== value 1
       , is (74) $ out <== value 1
       , is (73) $ out <== value 1
       , is (72) $ out <== value 1
       , is (71) $ out <== value 1
       , is (70) $ out <== value 1
       , is (69) $ out <== value 1
       , is (68) $ out <== value 1
       , is (67) $ out <== value 1
       , is (66) $ out <== value 1
       , is (65) $ out <== value 1
       , is (64) $ out <== value 1
       , is (63) $ out <== value 1
       , is (62) $ out <== value 1
       , is (61) $ out <== value 1
       , is (60) $ out <== value 1
       , is (59) $ out <== value 1
       , is (58) $ out <== value 1
       , is (57) $ out <== value 1
       , is (56) $ out <== value 2
       , is (55) $ out <== value 2
       , is (54) $ out <== value 2
       , is (53) $ out <== value 2
       , is (52) $ out <== value 2
       , is (51) $ out <== value 2
       , is (50) $ out <== value 2
       , is (49) $ out <== value 2
       , is (48) $ out <== value 2
       , is (47) $ out <== value 2
       , is (46) $ out <== value 2
       , is (45) $ out <== value 2
       , is (44) $ out <== value 3
       , is (43) $ out <== value 3
       , is (42) $ out <== value 3
       , is (41) $ out <== value 3
       , is (40) $ out <== value 3
       , is (39) $ out <== value 3
       , is (38) $ out <== value 3
       , is (37) $ out <== value 3
       , is (36) $ out <== value 4
       , is (35) $ out <== value 4
       , is (34) $ out <== value 4
       , is (33) $ out <== value 4
       , is (32) $ out <== value 4
       , is (31) $ out <== value 5
       , is (30) $ out <== value 5
       , is (29) $ out <== value 6
       , is (28) $ out <== value 6
       , is (27) $ out <== value 7
       , is (26) $ out <== value 7
       , is (25) $ out <== value 8
       , is (24) $ out <== value 8
       , is (23) $ out <== value 9
       , is (22) $ out <== value 9
       , is (21) $ out <== value 10
       , is (20) $ out <== value 10
       , is (19) $ out <== value 11
       , is (18) $ out <== value 11
       , is (17) $ out <== value 12
       , is (16) $ out <== value 12
       , is (15) $ out <== value 13
       , is (14) $ out <== value 14
       , is (13) $ out <== value 15
       , is (12) $ out <== value 16
       , is (11) $ out <== value 18
       , is (10) $ out <== value 20
       , is (9) $ out <== value 22
       , is (8) $ out <== value 24
       , is (7) $ out <== value 26
       , is (6) $ out <== value 28
       , is (5) $ out <== value 30
       , is (4) $ out <== value 32
       , is (3) $ out <== value 40
       , is (2) $ out <== value 47
       , is (1) $ out <== value 79
       ]
  
--------------------------------------------------------------------------------

testLUT = HW.icompile prog
  where
    prog :: Hardware ()
    prog = do component lut_signature
              return ()

--------------------------------------------------------------------------------
-- ** Carry Lookahead adder

adder_signature :: Signature (
     Signal (Bits 8)
  -> Signal (Bits 8)
  -> Signal (Bits 8)
  -> ()
  )
adder_signature =
  input  $ \a ->
  input  $ \b ->
  output $ \c ->
  ret $ adder a b c

adder :: Signal (Bits 8) -> Signal (Bits 8) -> Signal (Bits 8) -> Hardware ()
adder a b out =
  do cg <- component carry_generator_signature
     ha <- component half_adder_signature

     p :: Signal (Bits 8) <- signal "tempP"
     g :: Signal (Bits 8) <- signal "tempG"
     c :: Signal (Bits 9) <- signal "tempC"

     portmap ha (a .> b .> p .> g .> nill)
     portmap cg (p .> g .> c .> nill)
     
     return ()

--------------------------------------------------------------------------------

testAdder = HW.icompile prog
  where
    prog :: Hardware ()
    prog = do component adder_signature
              return ()

--------------------------------------------------------------------------------
-- Helpers.

carry_generator_signature :: Signature (
     Signal (Bits 8)
  -> Signal (Bits 8)
  -> Signal (Bits 9)
  -> ()
  )
carry_generator_signature =
  input  $ \a ->
  input  $ \b ->
  output $ \c ->
  ret $ carry_generator a b c

carry_generator :: Signal (Bits 8) -> Signal (Bits 8) -> Signal (Bits 9) -> Hardware ()
carry_generator p g c = process (p .: g .: []) $
  do let zero  = value 0 :: HData Bit
         one   = value 1 :: HData Integer
         seven = value 7 :: HData Integer
         ix    = value 0 :: HData Integer
     setArray ix zero c
     for seven $ \i ->
       do u    <- getArray i g
          v    <- getArray i p
          prev <- getArray i c
          setArray (i `plus` one) (u `or` (v `and` prev)) c

half_adder_signature :: Signature (
     Signal (Bits 8)
  -> Signal (Bits 8)
  -> Signal (Bits 8)
  -> Signal (Bits 8)
  -> ()
  )
half_adder_signature =
  input  $ \a ->
  input  $ \b ->
  output $ \p ->
  output $ \g ->
  ret $ half_adder a b p g

half_adder :: Signal (Bits 8) -> Signal (Bits 8) -> Signal (Bits 8) -> Signal (Bits 8) -> Hardware ()
half_adder a b p g = process (a .: b .: []) $
  do u <- unsafeFreezeSig a
     v <- unsafeFreezeSig b
     p <== (u `xor` v)
     g <== (u .&&.  v)

--------------------------------------------------------------------------------

testCarryGenerator = HW.icompile prog
  where
    prog :: Hardware ()
    prog = do component carry_generator_signature
              return ()

testHalfAdder = HW.icompile prog
  where
    prog :: Hardware ()
    prog = do component half_adder_signature
              return ()

--------------------------------------------------------------------------------
