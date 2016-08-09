{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Data structures for working with arrays
module Feldspar.Data.Array where


import Prelude (Functor, Foldable, Traversable, error, product, reverse)

import Data.Proxy

import Feldspar.Run



-- | Nested data structure (see explanation of 'nest')
data Nest a = Nest
    { nestNumSegs   :: Data Length
    , nestSegLength :: Data Length
    , nestInner     :: a
    }
  deriving (Functor, Foldable, Traversable)

instance Slicable a => Indexed (Nest a)
  where
    type IndexedElem (Nest a) = a
    Nest l w a ! i = slice (w*i') w a
      where
        i' = guardValLabel InternalAssertion (i<l) "invalid Nest slice" i

instance Finite (Nest a)
  where
    length (Nest l _ _) = l

instance Slicable a => Slicable (Nest a)
  where
    slice from n (Nest l w a) = Nest n' w $ slice (from'*w) (n'*w) a
      where
        guard = guardValLabel InternalAssertion (from+n<=l) "invalid Nest slice"
        from' = guard from
        n'    = guard n

instance Forcible a => Forcible (Nest a)
  where
    type ValueRep (Nest a) = (Data Length, Data Length, ValueRep a)
    toValue (Nest h w a) = toValue (h,w,a)
    fromValue v = let (h,w,a) = fromValue v in Nest h w a

instance Storable a => Storable (Nest a)
  where
    type StoreRep (Nest a)  = (Ref Length, Ref Length, StoreRep a)
    type StoreSize (Nest a) = StoreSize a
    newStoreRep _ sz =
        newStoreRep (Proxy :: Proxy (Data Length, Data Length, a)) ((),(),sz)
    initStoreRep (Nest h w a) = initStoreRep (h,w,a)
    readStoreRep rep = do
      (h,w,a) <- readStoreRep rep
      return $ Nest h w a
    unsafeFreezeStoreRep rep = do
      (h,w,a) <- unsafeFreezeStoreRep rep
      return $ Nest h w a
    writeStoreRep rep (Nest h w a) = writeStoreRep rep (h,w,a)
    copyStoreRep _ = copyStoreRep (Proxy :: Proxy (Data Length, Data Length, a))

-- | Note that @`HaskellRep` (`Nest` a) = (`Length`, `Length`, `HaskellRep` a)@
-- rather than @[HaskellRep a]@. This means that e.g.
-- @`Nest` (`Nest` (`Fin` (`IArr` a)))@ is represented as
-- @(Length,Length,(Length,Length,(Length,[...])))@ instead of the more
-- convenient @[[...]]@.
instance MarshalFeld a => MarshalFeld (Nest a)
  where
    type HaskellRep (Nest a) = (Length, Length, HaskellRep a)
    fromFeld (Nest h w a) = fromFeld (h,w,a)
    toFeld = do
        (h,w,a) <- toFeld
        return $ Nest h w a
  -- The reason for not using `HaskellRep (Nest a) = [HaskellRep a]` is that
  -- this representation makes it impossible to implement `toFeld`.

-- | Add a layer of nesting to a linear data structure by virtually chopping it
-- up into segments. The nesting is virtual in the sense that
-- @`unnest` (`nest` h w a)@ is syntactically identical to @a@.
--
-- In an expression @`nest` l w a@, it must be the case that
-- @l*w == `length` a@.
--
-- 'multiNest' may be a more convenient alternative to 'nest', expecially for
-- adding several levels of nesting.
nest :: Finite a
    => Data Length  -- ^ Number of segments
    -> Data Length  -- ^ Segment length
    -> a
    -> Nest a
nest l w a = Nest (guard l) (guard w) a
  where
    guard = guardValLabel
      InternalAssertion
      (l*w == length a)
      "nest: unbalanced nesting"

-- TODO Should `Nest` not be exported?

-- | A version of 'nest' that only takes the segment length as argument. The
-- total number of segments is computed by division.
--
-- In an expression @`nestEvery` n a@, it must be the case that
-- @div (`length` a) n * n == `length` a@.
--
-- This assumption permits removing the division in many cases when the nested
-- structure is later flattened in some way.
nestEvery :: Finite a
    => Data Length  -- ^ Segment length
    -> a
    -> Nest a
nestEvery n a = Nest (length a `unsafeBalancedDiv` n) n a

-- | Remove a layer of nesting
unnest :: Slicable a => Nest a -> a
unnest (Nest l w a) = slice 0 (l*w) a

-- | Increase dimensionality
--
-- This type is used to represent the number of dimensions of a
-- multi-dimensional structure. For example, @`Dim` (`Dim` ())@ means two
-- dimensions (see the aliases 'Dim1', 'Dim2', etc.).
data Dim d

-- | One dimension
type Dim1 = Dim ()

-- | Two dimensions
type Dim2 = Dim Dim1

-- | Three dimensions
type Dim3 = Dim Dim2

-- | Four dimensions
type Dim4 = Dim Dim3

-- | A description of the inner extent of a rectangular multi-dimensional
-- structure. \"Inner extent\" means the extent of all but the outermost
-- dimension.
--
-- For example, this value
--
-- @
-- `Outer` `:>` 10 `:>` 20 :: `InnerExtent` (`Dim` (`Dim` (`Dim` ())))
-- @
--
-- describes a three-dimensional structure where each inner structure has 10
-- rows and 20 columns.
data InnerExtent d
  where
    NoExt :: InnerExtent ()
    Outer :: InnerExtent (Dim ())
    (:>)  :: InnerExtent (Dim d) -> Data Length -> InnerExtent (Dim (Dim d))

infixl 5 :>

-- | Return the inner extent as a list of lengths
listExtent :: InnerExtent d -> [Data Length]
listExtent = reverse . go
  where
    go :: InnerExtent d -> [Data Length]
    go NoExt    = []
    go Outer    = []
    go (e :> l) = l : go e

-- | Add as much nesting to a one-dimensional structure as needed to reach the
-- given dimensionality
type family MultiNest d a
  where
    MultiNest (Dim ())      a = a
    MultiNest (Dim (Dim d)) a = Nest (MultiNest (Dim d) a)

-- | Turn a one-dimensional structure into a multi-dimensional one by adding
-- nesting as described by the given 'InnerExtent'
multiNest :: forall a d . Finite a =>
    InnerExtent (Dim d) -> a -> MultiNest (Dim d) a
multiNest e a = go e lsAll
  where
    lsInner = listExtent e
    lsAll   = unsafeBalancedDiv (length a) (product lsInner) : lsInner
      -- Extent of *all* dimensions (including the outermost)

    go :: InnerExtent (Dim d') -> [Data Length] -> MultiNest (Dim d') a
    go Outer    _          = a
    go (e :> _) (l1:l2:ls) = Nest l1 l2 $ go e (l1*l2 : ls)
    go (e :> _) _          = error "impossible"
      -- Note: The `InnerExtent` argument is just there for the type checker. We
      -- cannot take the lengths from that value, because they come in the wrong
      -- order.

-- | A version of 'InnerExtent' for internal use
data InnerExtent' d
  where
    ZE :: InnerExtent' ()
    OE :: InnerExtent' (Dim ())
    SE :: Data Length -> InnerExtent' d -> InnerExtent' (Dim d)
  -- `InnerExtent'` is more convenient to work with than `InnerExtent`, because
  -- it recurses over the dimensions outside-in. However, `InnerExtent` is more
  -- convenient for the user. Consider these two values describing the inner
  -- extent of a three-dimensional structure:
  --
  --     Outer :> 10 :> 20
  --     10 `SE` (20 `SE` ZE)
  --
  -- In the first case it's clear that the extent of the outermost dimension is
  -- omitted.

listExtent' :: InnerExtent' d -> [Data Length]
listExtent' ZE       = []
listExtent' OE       = []
listExtent' (SE l e) = l : listExtent' e

tailExtent' :: InnerExtent' (Dim d) -> InnerExtent' d
tailExtent' OE        = ZE
tailExtent' (SE _ ls) = ls

convInnerExtent :: InnerExtent d -> InnerExtent' d
convInnerExtent e = go e (listExtent e)
  where
    go :: InnerExtent d -> [Data Length] -> InnerExtent' d
    go NoExt    _      = ZE
    go Outer    _      = OE
    go (e :> _) (l:ls) = SE l $ go e ls
    go (_ :> _) _      = error "convInnerExtent: impossible"

