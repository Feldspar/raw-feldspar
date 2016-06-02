{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Data structures for working with arrays
module Feldspar.Data.Array where



import Feldspar.Run



--------------------------------------------------------------------------------
-- * Finite linear structures
--------------------------------------------------------------------------------

-- | Make a dimension-less value finite by pairing it with a length
--
-- A typical use of 'Fin' is @`Fin` (`IArr` a)@.
data Fin a = Fin
    { finLength :: Data Length
    , fin_inner :: a
    }
  deriving (Functor, Foldable, Traversable)

instance Indexed a => Indexed (Fin a)
  where
    type IndexedElem (Fin a) = IndexedElem a
    Fin _ a ! i = a!i

instance Indexed a => Finite (Fin a)
  where
    length = finLength

instance Slicable a => Slicable (Fin a)
  where
    slice from n (Fin len a) = Fin n $ slice from n a

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) =>
    MarshalFeld (Fin (Arr a))
  where
    type HaskellRep (Fin (Arr a)) = [a]

    fromFeld (Fin len arr) = do
        fput stdout "" len " "
        for (0,1,Excl len) $ \i -> do
            a <- getArr i arr
            fromFeld (a :: Data a)
            printf " "

    toFeld = do
        len <- fget stdin
        arr <- newArr len
        for (0,1,Excl len) $ \i -> do
            a <- toFeld
            setArr i (a :: Data a) arr
        return $ Fin len arr

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) =>
    MarshalFeld (Fin (IArr a))
  where
    type HaskellRep (Fin (IArr a)) = [a]

    fromFeld (Fin len arr) = do
        fput stdout "" len " "
        for (0,1,Excl len) $ \i -> do
            fromFeld (arrIx arr i :: Data a)
            printf " "

    toFeld = do
        len <- fget stdin
        arr <- newArr len
        for (0,1,Excl len) $ \i -> do
            a <- toFeld
            setArr i (a :: Data a) arr
        iarr <- unsafeFreezeArr arr
        return $ Fin len iarr



--------------------------------------------------------------------------------
-- * Nested data structures
--------------------------------------------------------------------------------

-- | Nested data structure (see explanation of 'nest')
data Nest a = Nest
    { nestNumSegs   :: Data Length
    , nestSegLength :: Data Length
    , nestInner     :: a
    }

instance Slicable a => Indexed (Nest a)
  where
    type IndexedElem (Nest a) = a
    Nest _ w a ! i = slice (w*i) (w*i+w) a

instance Finite (Nest a)
  where
    length (Nest l _ _) = l

instance Slicable a => Slicable (Nest a)
  where
    slice from n (Nest _ w man) = Nest n w $ slice (from*w) (n*w) man

-- | Add a layer of nesting to a linear data structure by symbolically chopping
-- it up into segments. The nesting is symbolic in the sense that
-- @`unnest` (`nest` h w a)@ is syntactically identical to @a@.
--
-- In an expression @`nest` l w a@, it must be the case that
-- @l*w == `length` a@.
--
-- 'multiNest' may be a more convenient alternative to 'nest', expecially for
-- adding several levels of nesting.
nest
    :: Data Length  -- Number of segments
    -> Data Length  -- Segment length
    -> a
    -> Nest a
nest = Nest
  -- TODO Assert w == length a

-- | Remove a layer of nesting
unnest :: Nest a -> a
unnest (Nest _ _ a) = a

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

-- | A description of the extent of a rectangular multi-dimensional structure
--
-- For example, this value
--
-- @
-- 10 :> 20 :> Z :: Extent (Dim (Dim ()))
-- @
--
-- describes a two-dimensional structure with 10 rows and 20 columns.
data Extent d
  where
    ZE   :: Extent ()
    (:>) :: Data Length -> Extent d -> Extent (Dim d)

infixr 5 :>

-- | Get a list of the extent in each dimension
listExtent :: Extent d -> [Data Length]
listExtent ZE       = []
listExtent (l :> e) = l : listExtent e

-- | Add as much nesting to a one-dimensional structure as needed to reach the
-- given dimensionality
type family MultiNest d a
  where
    MultiNest (Dim ()) a = a
    MultiNest (Dim d)  a = Nest (MultiNest d a)

-- | Turn a one-dimensional structure into a multi-dimensional one by adding
-- nesting as described by the given 'Extent'
multiNest
    :: Extent (Dim d)  -- ^ Extent of the result
    -> a               -- ^ One-dimensional structure
    -> MultiNest (Dim d) a
multiNest (l :> ZE)       a = a  -- TODO Assert l == length a
multiNest (l1 :> l2 :> e) a = Nest l1 l2 $ multiNest (l2 :> e) a

