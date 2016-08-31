{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | This module gives a library of different vector types.
--
-- = Basic use
--
-- A typical 1-dimensional vector computation goes as follows:
--
-- 1. Start with a 'Manifest' vector (one that is refers directly to an array in
--   memory).
--
-- 2. Apply operations overloaded by 'Pully' (e.g. 'take', 'drop', 'map',
--    'reverse'). The result is one or more 'Pull' vectors.
--
-- 3. If the previous step resulted in several parts, assemble them using
--    operations overloaded by 'Pushy' (e.g. '++'). The result is a 'Push'
--    vector.
--
-- 4. Write the vector to memory using 'manifest' or 'manifestFresh'.
--
-- (Of course, there are many variations on this general scheme.)
--
-- Note that it's possible to skip step \#2 or \#3 above. For example, it's
-- possible to directly concatenate two 'Manifest' vectors using '++', and
-- 'manifest' can be applied directly to a 'Pull' vector (or even to a
-- 'Manifest', in which case it becomes a no-op).
--
--
--
-- = Efficiency and fusion
--
-- The library has been designed so that all operations fuse together without
-- creating any intermediate structures in memory. The only exception is the
-- operations that produce 'Manifest' or 'Manifest2' vectors ('manifest',
-- 'manifest2', etc.).
--
-- For example, the following function only creates a single structure in memory
-- even though it seemingly generates several intermediate vectors:
--
-- @
-- f :: (`Num` a, `Syntax` a, `MonadComp` m) => `Pull` a -> m (`Manifest` a)
-- f = `manifestFresh` . `reverse` . `map` (*2) . `take` 10
-- @
--
-- Furthermore, the operations associated with each type of vector are
-- restricted to operations that can be carried out efficiently for that type.
-- For example, although it would be possible to implement append for 'Pull'
-- vectors, doing so results in unnecessary conditionals in the generated code.
-- Therefore, the '++' operator returns a 'Push' vector, which ensures efficient
-- generated code.
--
-- In many cases, the cycle 'Manifest' -> 'Pull' -> 'Push' -> 'Manifest' is
-- guided by the types of the operations involved. However, there are cases when
-- it's preferable to shortcut the cycle even when it's not demanded by the
-- types. The reason is that fusion can lead to duplicated computations.
--
-- Here is an example where fusion leads to redundant computations:
--
-- @
-- bad = do
--     v :: `DManifest` `Int32` <- `readStd`  -- Read from stdin
--     let v'  = `map` heavy v
--         v'' = v' `++` `reverse` v'
--     `writeStd` v''                     -- Write to stdout
-- @
--
-- Since @v'@ is used twice in defining @v''@, the mapping of the @heavy@
-- computation will be done twice when writing @v''@ to the output. One way to
-- prevent this is to perform the heavy mapping once, store the result in
-- memory, and define @v''@ from the stored vector:
--
-- @
-- good = do
--     v :: `DManifest` `Int32` <- `readStd`  -- Read from stdin
--     v' <- `manifestFresh` $ `map` heavy v
--     let v'' = v' `++` `reverse` v'
--     `writeStd` v''                     -- Write to stdout
-- @
--
-- Even though the examples are called @bad@ and @good@, there's not a clear-cut
-- answer to which version is best. It could depend on whether time or
-- memory is the most scarce resource. This library leaves the decision in the
-- hands of the programmer.
--
--
--
-- = Working with matrices
--
-- 2-dimensional matrix computations can follow a scheme similar to the above by
-- using the types 'Manifest2', 'Pull2' and 'Push2' and the corresponding
-- operations.
--
-- A quite common situation is the need to apply an operation on each row or
-- column of a matrix. Operating on the rows can be done by a combination of
-- 'exposeRows' and 'hideRows'. For example, this function reverses each row in
-- a matrix:
--
-- @
-- revEachRow :: `MonadComp` m => `Pull2` a -> `Push2` m a
-- revEachRow mat = `hideRows` (`numCols` mat) $ `map` `reverse` $ `exposeRows` mat
-- @
--
-- 'exposeRows' takes a 'Pully2' matrix and turns it into @`Pull` (`Pull` a)@
-- i.e. a vector of row vectors. 'map' is used to apply 'reverse' to each row.
-- Finally, 'hideRows' turns the nested vector it back into a matrix, of type
-- 'Push2'.
--
-- Note that 'hideRows' generally cannot know the length of the rows, so this
-- number has to be provided as its first argument. When compiling with
-- assertions, it will be checked at runtime that the length of each row is
-- equal to the given length.
--
-- In order to operate on the columns instead of the rows, just apply
-- 'transpose' on the original matrix. This operation will fuse with the rest of
-- the computation.
--
-- It gets a bit more complicated when the operation applied to each row is
-- effectful. For example, the operation may have to use 'manifest' internally
-- giving it a monadic result type. In such situations, the function 'sequens'
-- is helpful. It is a bit similar to the standard function @sequence@ for
-- lists, execept that it converts @`Push` m (m a)@ into @`Push` m a@; i.e. it
-- embeds the effect into the resulting 'Push' vector.
--
-- Here is a version of the previous example where the row operation is
-- effectful (due to 'manifestFresh') and 'sequens' is inserted to embed the
-- effects:
--
-- @
-- revEachRowM :: (`Syntax` a, `MonadComp` m) => `Pull2` a -> `Push2` m a
-- revEachRowM mat = `hideRows` (`numCols` mat) $ `sequens`
--                 $ `map` (`manifestFresh` . `reverse`) $ `exposeRows` mat
--
-- @
--
-- Note that 'sequens' is generally a dangerous function due to the hiding of
-- effects inside the resulting vector. These effects may be (seemingly)
-- randomly interleaved with other effects when the vector is used. However, the
-- above example is fine, since 'manifestFresh' allocates a fresh array for the
-- storage, so its effects cannot be observed from the outside.
--
-- The comments to 'Push' elaborate more on the semantics of push vectors with
-- interleaved effects.

module Feldspar.Data.Vector
  ( module Feldspar.Data.Array
  , module Feldspar.Data.Vector
  ) where



import qualified Prelude

import Data.List (genericLength)
import Data.Proxy

import Feldspar
import Feldspar.Data.Array
import Feldspar.Run
import Feldspar.Run.Concurrent
import qualified Language.Embedded.Concurrent as Imp



-- This library has been inspired by the vector library in feldspar-language:
-- <https://github.com/Feldspar/feldspar-language/blob/master/src/Feldspar/Vector.hs>
--
-- The general idea of pull and push vectors is described in
-- "Combining deep and shallow embedding of domain-specific languages"
-- <http://dx.doi.org/10.1016/j.cl.2015.07.003>.
--
-- Push arrays were originally introduced in
-- "Expressive array constructs in an embedded GPU kernel programming language"
-- <http://dx.doi.org/10.1145/2103736.2103740>.



--------------------------------------------------------------------------------
-- * 1-dimensional manifest vectors
--------------------------------------------------------------------------------

-- | A 1-dimensional vector with a concrete representation in memory
--
-- There are two main reasons to use 'Manifest' when possible instead of `Pull`:
--
-- * The operations of the 'Manifestable' class are more efficient for
--   'Manifest'. They either result in a no-op or an efficient memory-copy
--   (instead of a copying loop).
--
-- * 'Manifest' can be freely converted to/from a 2-dimensional structure using
--   'nest' and 'unnest'. Note that the representation of 'Manifest2' is
--   @`Nest` (`Manifest` a)@.
type Manifest = IArr

-- | 'Manifest' vector specialized to 'Data' elements
type DManifest a = DIArr a

-- | Treated as a row vector
instance Finite2 (Manifest a) where extent2 v = (1, length v)

-- | Make a 'Manifest' vector from a list of values
listManifest :: (Syntax a, MonadComp m) => [a] -> m (Manifest a)
listManifest = manifestFresh . listPush



--------------------------------------------------------------------------------
-- * 2-dimensional manifest vectors
--------------------------------------------------------------------------------

-- | A 2-dimensional vector with a concrete representation in memory
type Manifest2 a = Nest (Manifest a)

-- | 'Manifest2' vector specialized to 'Data' elements
type DManifest2 a = Manifest2 (Data a)



--------------------------------------------------------------------------------
-- * 1-dimensional pull vectors
--------------------------------------------------------------------------------

-- | 1-dimensional pull vector: a vector representation that supports random
-- access and fusion of operations
data Pull a where
    Pull
        :: Data Length        -- Length of vector
        -> (Data Index -> a)  -- Index function
        -> Pull a

-- | 'Pull' vector specialized to 'Data' elements
type DPull a = Pull (Data a)

instance Functor Pull
  where
    fmap f (Pull len ixf) = Pull len (f . ixf)

-- It would be possible to have the instance:
--
--     instance Applicative Pull
--       where
--         pure a  = Pull 1 (const a)
--         Pull len1 ixf1 <*> Pull len2 ixf2 = Pull (len1*len2) $ \i ->
--             let k = i `div` len2
--                 l = i `mod` len2
--             in  ixf1 k $ ixf2 l
--
-- However, it has been omitted due to the use of `div` and `mod`.

instance Indexed (Pull a)
  where
    type IndexedElem (Pull a) = a
    Pull len ixf ! i = ixf $ guardValLabel
      InternalAssertion
      (i < len)
      "indexing outside of Pull vector"
      i

instance Finite (Pull a)
  where
    length (Pull len _) = len

-- | Treated as a row vector
instance Finite2 (Pull a) where extent2 v = (1, length v)

instance Slicable (Pull a)
  where
    slice from n = take n . drop from

instance
    ( Syntax a
    , MarshalHaskell (Internal a)
    , MarshalFeld a
    ) =>
      MarshalFeld (Pull a)
  where
    type HaskellRep (Pull a) = HaskellRep (Manifest a)
    fwrite hdl = fwrite hdl . toSeq
    fread hdl  = (toPull :: Manifest a -> _) <$> fread hdl

data VecChanSizeSpec lenSpec = VecChanSizeSpec (Data Length) lenSpec

ofLength :: Data Length -> lenSpec -> VecChanSizeSpec lenSpec
ofLength = VecChanSizeSpec

instance ( Syntax a, BulkTransferable a
         , ContainerType a ~ Arr a
         ) => Transferable (Pull a)
  where
    type SizeSpec (Pull a) = VecChanSizeSpec (SizeSpec a)
    calcChanSize _ (VecChanSizeSpec n m) =
        let hsz = n `Imp.timesSizeOf` (Proxy :: Proxy Length)
            bsz = calcChanSize (Proxy :: Proxy a) m
        in  hsz `Imp.plusSize` (n `Imp.timesSize` bsz)
    untypedReadChan c = do
        len :: Data Length <- untypedReadChan c
        arr <- newArr len
        untypedReadChanBuf (Proxy :: Proxy a) c 0 len arr
        toPull <$> unsafeFreezeArr arr
    untypedWriteChan c v = do
        arr <- newArr len
        untypedWriteChan c len
        untypedWriteChanBuf (Proxy :: Proxy a) c 0 len arr
      where
        len = length v
 -- TODO Make instances for other vector types

-- | Data structures that are 'Pull'-like (i.e. support '!' and 'length')
class    (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a
instance (Indexed vec, Finite vec, IndexedElem vec ~ a) => Pully vec a

-- | Convert a vector to 'Pull'
toPull :: Pully vec a => vec -> Pull a
toPull vec = Pull (length vec) (vec!)



----------------------------------------
-- ** Operations
----------------------------------------

-- | Take the head of a non-empty vector
head :: Pully vec a => vec -> a
head = (!0)

tail :: Pully vec a => vec -> Pull a
tail = drop 1

take :: Pully vec a => Data Length -> vec -> Pull a
take l vec = Pull (min (length vec) l) (vec!)

drop :: Pully vec a => Data Length -> vec -> Pull a
drop l vec = Pull (b2i (l<=m) * (m-l)) ((vec!) . (+l))
  where
    m = length vec

tails :: Pully vec a => vec -> Pull (Pull a)
tails vec = Pull (length vec + 1) (`drop` vec)

inits :: Pully vec a => vec -> Pull (Pull a)
inits vec = Pull (length vec + 1) (`take` vec)

inits1 :: Pully vec a => vec -> Pull (Pull a)
inits1 = tail . inits

replicate :: Data Length -> a -> Pull a
replicate l = Pull l . const

map :: Pully vec a => (a -> b) -> vec -> Pull b
map f vec = Pull (length vec) (f . (vec!))

zip :: (Pully vec1 a, Pully vec2 b) => vec1 -> vec2 -> Pull (a,b)
zip a b = Pull (length a `min` length b) (\i -> (a!i, b!i))

-- | Back-permute a 'Pull' vector using an index mapping. The supplied mapping
-- must be a bijection when restricted to the domain of the vector. This
-- property is not checked, so use with care.
backPermute :: Pully vec a =>
    (Data Length -> Data Index -> Data Index) -> (vec -> Pull a)
backPermute perm vec = Pull len ((vec!) . perm len)
  where
    len = length vec

reverse :: Pully vec a => vec -> Pull a
reverse = backPermute $ \len i -> len-i-1

(...) :: Data Index -> Data Index -> Pull (Data Index)
l ... h = Pull (b2i (l<h+1) * (h-l+1)) (+l)

infix 3 ...

zipWith :: (Pully vec1 a, Pully vec2 b) =>
    (a -> b -> c) -> vec1 -> vec2 -> Pull c
zipWith f a b = fmap (uncurry f) $ zip a b

-- | Left fold of a vector
fold :: (Syntax a, Pully vec a) => (a -> a -> a) -> a -> vec -> a
fold f init vec = forLoop (length vec) init $ \i st -> f (vec!i) st

-- | Left fold of a non-empty vector
fold1 :: (Syntax a, Pully vec a) => (a -> a -> a) -> vec -> a
fold1 f vec = forLoop (length vec) (vec!0) $ \i st -> f (vec!(i+1)) st

sum :: (Pully vec a, Syntax a, Num a) => vec -> a
sum = fold (+) 0

-- | Scalar product
scProd :: (Num a, Syntax a, Pully vec1 a, Pully vec2 a) => vec1 -> vec2 -> a
scProd a b = sum (zipWith (*) a b)



--------------------------------------------------------------------------------
-- * 2-dimensional pull vectors
--------------------------------------------------------------------------------

-- | 2-dimensional pull vector: a vector representation that supports random
-- access and fusion of operations
data Pull2 a where
    Pull2
        :: Data Length                      -- Number of rows
        -> Data Length                      -- Number of columns
        -> (Data Index -> Data Index -> a)  -- (row,col) -> element
        -> Pull2 a

-- | 'Pull2' vector specialized to 'Data' elements
type DPull2 a = Pull2 (Data a)

instance Functor Pull2
  where
    fmap f (Pull2 r c ixf) = Pull2 r c (\i j -> f $ ixf i j)

-- | Indexing the rows
instance Indexed (Pull2 a)
  where
    type IndexedElem (Pull2 a) = Pull a
    Pull2 r c ixf ! i = Pull c (ixf i')
      where
        i' = guardValLabel
          InternalAssertion
          (i < r)
          "indexing outside of Pull2 vector"
          i

-- | 'length' gives number of rows
instance Finite (Pull2 a) where length = numRows

instance Finite2 (Pull2 a)
  where
    extent2 (Pull2 r c _) = (r,c)

-- | Take a slice of the rows
instance Slicable (Pull2 a)
  where
    slice from n vec
        = hideRowsPull (numCols vec)
        $ take n
        $ drop from
        $ exposeRows
        $ vec

instance
    ( Syntax a
    , MarshalHaskell (Internal a)
    , MarshalFeld a
    ) =>
      MarshalFeld (Pull2 a)
  where
    type HaskellRep (Pull2 a) = HaskellRep (Manifest2 a)
    fwrite hdl = fwrite hdl . toPush2
    fread hdl  = (toPull2 :: Manifest2 a -> _) <$> fread hdl

-- | Vectors that can be converted to 'Pull2'
class Pully2 vec a | vec -> a
  where
    -- | Convert a vector to 'Pull2'
    toPull2 :: vec -> Pull2 a

-- | Convert to a 'Pull2' with a single row
instance Syntax a => Pully2 (Manifest a) a
  where
    toPull2 = toPull2 . toPull

instance (Indexed vec, Slicable vec, IndexedElem vec ~ a, Syntax a) =>
    Pully2 (Nest vec) a
  where
    toPull2 arr = Pull2 r c $ \i j -> arr!i!j
      where
        (r,c) = extent2 arr

-- | Convert to a 'Pull2' with a single row
instance Pully2 (Pull a) a
  where
    toPull2 (Pull l ixf) = Pull2 1 l $ \_ j -> ixf j

instance Pully2 (Pull2 a) a where toPull2 = id



----------------------------------------
-- ** Operations
----------------------------------------

-- | Transposed version of 'toPull'. Can be used to e.g. turn a 'Pull' into a
-- column of a matrix
toPull2' :: Pully2 vec a => vec -> Pull2 a
toPull2' = transpose . toPull2

-- | Turn a vector of rows into a 2-dimensional vector. All inner vectors are
-- assumed to have the given length, and this assumption is not checked by
-- assertions. If types permit, it is preferable to use 'hideRows', which does
-- check the lengths.
hideRowsPull :: (Pully vec1 vec2, Pully vec2 a)
    => Data Length  -- ^ Length of inner vectors
    -> vec1
    -> Pull2 a
hideRowsPull c vec = Pull2 (length vec) c $ \i j -> vec!i!j

-- | Expose the rows in a 'Pull2' by turning it into a vector of rows
exposeRows :: Pully2 vec a => vec -> Pull (Pull a)
exposeRows vec = Pull (numRows v) $ \i -> Pull (numCols v) $ \j -> v!i!j
  where
    v = toPull2 vec

-- | Transpose of a matrix
transpose :: Pully2 vec a => vec -> Pull2 a
transpose vec = Pull2 (numCols v) (numRows v) $ \i j -> v!j!i
  where
    v = toPull2 vec

toRowVec :: Pully vec a => vec -> Pull2 a
toRowVec vec = hideRowsPull (length vec) $ replicate 1 vec

fromRowVec :: Pully2 vec a => vec -> Pull a
fromRowVec = head . exposeRows

toColVec :: Pully vec a => vec -> Pull2 a
toColVec = transpose . toRowVec

fromColVec :: Pully2 vec a => vec -> Pull a
fromColVec = fromRowVec . transpose

-- | Matrix multiplication
matMul :: (Pully2 vec1 a, Pully2 vec2 a, Num a, Syntax a) =>
    vec1 -> vec2 -> Pull2 a
matMul veca vecb = Pull2 (numRows va) (numCols vb) $ \i j ->
    scProd (va!i) (transpose vb ! j)
  where
    va = toPull2 veca
    vb = toPull2 vecb



--------------------------------------------------------------------------------
-- * 1-dimensional push vectors
--------------------------------------------------------------------------------

-- | 1-dimensional push vector: a vector representation that supports nested
-- write patterns (e.g. resulting from concatenation) and fusion of operations
--
-- If it is the case that @`dumpPush` v (\\_ _ -> `return` ())@ has the same
-- behavior as @`return` ()@, i.e., the vector does not have any embedded side
-- effects, we can regard 'Push' as a pure data structure with the denotation of
-- a finite list.
--
-- However, 'Push' is commonly used to assemble data after splitting it up and
-- performing some operations on the parts. We want to be able to use 'Push'
-- even if the operation involved has side effects. The function 'sequens' can
-- be used to embed effects into a 'Push' vector.
--
-- 'Push' vectors with embedded effects can often be considered to be denoted by
-- @M [a]@, where @M@ is some suitable monad. That is, the vector performs some
-- effects and produces a finite list of values as a result. This denotation is
-- enough to explain e.g. why
--
-- @
-- `return` (v `++` v)
-- @
--
-- is different from
--
-- @
-- do v' <- `manifestFresh` v
--    `return` (v' `++` v')
-- @
--
-- (The former duplicates the embedded effects while the latter only performs
-- the effects once.)
--
-- However, this denotation is not enough to model 'dumpPush', which allows a
-- write method to be interleaved with the embedded effects. Even a function
-- such as 'manifest' can to some extent be used observe the order of effects
-- (if the array argument to 'manifest' is also updated by the internal
-- effects).
--
-- Conclusion:
--
-- * You can normally think of @`Push` a@ as denoting @M [a]@ (finite list)
--
-- * Make sure to pass a free array as argument to 'manifest'
--
-- * Avoid using 'dumpPush' unless you know that it's safe
data Push m a
  where
    Push
        :: Data Length
        -> ((Data Index -> a -> m ()) -> m ())
        -> Push m a

-- | 'Push' vector specialized to 'Data' elements
type DPush m a = Push m (Data a)

instance Functor (Push m)
  where
    fmap f (Push len dump) = Push len $ \write ->
        dump $ \i -> write i . f

-- | This instance behaves like the list instance:
--
-- > pure x    = [x]
-- > fs <*> xs = [f x | f <- fs, x <- xs]
instance Applicative (Push m)
  where
    pure a = Push 1 $ \write -> write 0 a
    vec1 <*> vec2 = Push (len1*len2) $ \write -> do
        dumpPush vec2 $ \i2 a ->
          dumpPush vec1 $ \i1 f ->
            write (i1*len2 + i2) (f a)
      where
        (len1,len2) = (length vec1, length vec2)

-- No instance `Monad Push`, because it's not possible to determine the length
-- of the result of `>>=`.

instance Finite (Push m a)
  where
    length (Push len _) = len

-- | Treated as a row vector
instance Finite2 (Push m a) where extent2 v = (1, length v)

instance
    ( Syntax a
    , MarshalHaskell (Internal a)
    , MarshalFeld a
    , m ~ Run
    ) =>
      MarshalFeld (Push m a)
  where
    type HaskellRep (Push m a) = HaskellRep (Manifest a)
    fwrite hdl = fwrite hdl <=< manifestFresh
    fread hdl  = toPush . (id :: Manifest _ -> _) <$> fread hdl

-- | Vectors that can be converted to 'Push'
class Pushy m vec a | vec -> a
  where
    -- | Convert a vector to 'Push'
    toPush :: vec -> Push m a

-- | A version of 'toPush' that constrains the @m@ argument of 'Push' to that of
-- the monad in which the result is returned. This can be a convenient way to
-- avoid unresolved overloading.
toPushM :: (Pushy m vec a, Monad m) => vec -> m (Push m a)
toPushM = return . toPush

instance (Syntax a, MonadComp m) => Pushy m (Manifest a) a where toPush = toPush . toPull
instance (m1 ~ m2)               => Pushy m1 (Push m2 a) a where toPush = id

instance MonadComp m => Pushy m (Pull a) a
  where
    toPush vec = Push len $ \write ->
        for (0,1,Excl len) $ \i ->
          write i (vec!i)
      where
        len = length vec

instance (MonadComp m1, m1 ~ m2) => Pushy m1 (Seq m2 a) a
  where
    toPush (Seq len init) = Push len $ \write -> do
      next <- init
      for (0,1,Excl len) $ \i -> do
        a <- next i
        write i a

-- | Dump the contents of a 'Push' vector
dumpPush
    :: Push m a                   -- ^ Vector to dump
    -> (Data Index -> a -> m ())  -- ^ Function that writes one element
    -> m ()
dumpPush (Push _ dump) = dump



----------------------------------------
-- ** Operations
----------------------------------------

-- | Create a 'Push' vector from a list of elements
listPush :: Monad m => [a] -> Push m a
listPush as = Push (value $ genericLength as) $ \write ->
    sequence_ [write (value i) a | (i,a) <- Prelude.zip [0..] as]

-- | Append two vectors to make a 'Push' vector
(++) :: (Pushy m vec1 a, Pushy m vec2 a, Monad m) => vec1 -> vec2 -> Push m a
vec1 ++ vec2 = Push (len1 + length v2) $ \write ->
    dumpPush v1 write >> dumpPush v2 (write . (+len1))
  where
    v1   = toPush vec1
    v2   = toPush vec2
    len1 = length v1

-- Concatenate nested vectors to a 'Push' vector
concat :: (Pushy m vec1 vec2, Pushy m vec2 a, MonadComp m)
    => Data Length  -- ^ Length of inner vectors
    -> vec1
    -> Push m a
concat c vec = Push (len*c) $ \write ->
    dumpPush v $ \i row ->
      dumpPush row $ \j a -> do
        assertLabel
          InternalAssertion
          (length row == c)
          "concat: inner length differs"
        write (i * length row + j) a
  where
    v   = fmap toPush $ toPush vec
    len = length v

-- Flatten a 2-dimensional vector to a 'Push' vector
flatten :: Pushy2 m vec a => vec -> Push m a
flatten vec = Push (r*c) $ \write ->
    dumpPush2 v $ \i j -> write (i*c + j)
  where
    v     = toPush2 vec
    (r,c) = extent2 v

-- | Embed the effects in the elements into the internal effects of a 'Push'
-- vector
--
-- __WARNING:__ This function should be used with care, since it allows hiding
-- effects inside a vector. These effects may be (seemingly) randomly
-- interleaved with other effects when the vector is used.
--
-- The name 'sequens' has to do with the similarity to the standard function
-- 'sequence'.
sequens :: (Pushy m vec (m a), Monad m) => vec -> Push m a
sequens vec = Push (length v) $ \write ->
    dumpPush v $ \i m ->
      m >>= write i
  where
    v = toPush vec

-- | Forward-permute a 'Push' vector using an index mapping. The supplied
-- mapping must be a bijection when restricted to the domain of the vector. This
-- property is not checked, so use with care.
forwardPermute :: Pushy m vec a =>
    (Data Length -> Data Index -> Data Index) -> vec ->  Push m a
forwardPermute p vec = Push len $ \write ->
    dumpPush v $ \i a ->
      write (p len i) a
  where
    v   = toPush vec
    len = length v



--------------------------------------------------------------------------------
-- * 2-dimensional push vectors
--------------------------------------------------------------------------------

-- | 2-dimensional push vector: a vector representation that supports nested
-- write patterns (e.g. resulting from concatenation) and fusion of operations
--
-- See the comments to 'Push' regarding the semantics of push vectors with
-- interleaved effects.
data Push2 m a
  where
    Push2
        :: Data Length  -- Number of rows
        -> Data Length  -- Number of columns
        -> ((Data Index -> Data Index -> a -> m ()) -> m ())
        -> Push2 m a

-- | 'Push2' vector specialized to 'Data' elements
type DPush2 m a = Push2 m (Data a)

instance Functor (Push2 m)
  where
    fmap f (Push2 r c dump) = Push2 r c $ \write ->
        dump $ \i j -> write i j . f

-- | 'length' gives number of rows
instance Finite (Push2 m a)
  where
    length (Push2 r _ _) = r

instance Finite2 (Push2 m a)
  where
    extent2 (Push2 r c _) = (r,c)

instance
    ( Syntax a
    , MarshalHaskell (Internal a)
    , MarshalFeld a
    , m ~ Run
    ) =>
      MarshalFeld (Push2 m a)
  where
    type HaskellRep (Push2 m a) = HaskellRep (Manifest2 a)
    fwrite hdl = fwrite hdl <=< manifestFresh2
    fread hdl  = toPush2 . (id :: Manifest2 _ -> _) <$> fread hdl

-- | Vectors that can be converted to 'Push2'
class Pushy2 m vec a | vec -> a
  where
    -- | Convert a vector to 'Push2'
    toPush2 :: vec -> Push2 m a

-- | A version of 'toPush2' that constrains the @m@ argument of 'Push2' to that
-- of the monad in which the result is returned. This can be a convenient way to
-- avoid unresolved overloading.
toPushM2 :: (Pushy2 m vec a, Monad m) => vec -> m (Push2 m a)
toPushM2 = return . toPush2

-- | Convert to a 'Push2' with a single row
instance (Syntax a, MonadComp m) => Pushy2 m (Manifest a)  a where toPush2 = toPush2 . toPull
instance (Syntax a, MonadComp m) => Pushy2 m (Manifest2 a) a where toPush2 = toPush2 . toPull2
instance MonadComp m             => Pushy2 m (Pull a)      a where toPush2 = toPush2 . toPull2
instance (m1 ~ m2)               => Pushy2 m1 (Push2 m2 a) a where toPush2 = id

instance MonadComp m => Pushy2 m (Pull2 a) a
  where
    toPush2 vec = Push2 r c $ \write ->
        for (0,1,Excl r) $ \i ->
          for (0,1,Excl c) $ \j ->
          write i j (vec!i!j)
      where
        (r,c) = extent2 vec

-- | Dump the contents of a 'Push2' vector
dumpPush2
    :: Push2 m a                                -- ^ Vector to dump
    -> (Data Index -> Data Index -> a -> m ())  -- ^ Function that writes one element
    -> m ()
dumpPush2 (Push2 _ _ dump) = dump



----------------------------------------
-- ** Operations
----------------------------------------

-- | Turn a vector of rows into a 2-dimensional vector. All inner vectors are
-- assumed to have the given length.
hideRows :: (Pushy m vec1 vec2, Pushy m vec2 a, MonadComp m)
    => Data Length  -- ^ Length of inner vectors
    -> vec1
    -> Push2 m a
hideRows c vec = Push2 (length v) c $ \write ->
    dumpPush v $ \i row ->
      dumpPush row $ \j a -> do
        assertLabel
          InternalAssertion
          (length row == c)
          "hideRows: inner length differs"
        write i j a
  where
    v = fmap toPush $ toPush vec

-- | Convert a 2-dimensional vector with effectful elements to 'Push2'
--
-- __WARNING:__ This function should be used with care, since is allows hiding
-- effects inside a vector. These effects may be (seemingly) randomly
-- interleaved with other effects when the vector is used.
--
-- The name 'sequens2' has to do with the similarity to the standard function
-- 'sequence'.
sequens2 :: (Pushy2 m vec (m a), Monad m) => vec -> Push2 m a
sequens2 vec = Push2 (numRows v) (numCols v) $ \write ->
    dumpPush2 v $ \i j m ->
      m >>= write i j
  where
    v = toPush2 vec

-- | Forward-permute a 'Push' vector using an index mapping. The supplied
-- mapping must be a bijection when restricted to the domain of the vector. This
-- property is not checked, so use with care.
forwardPermute2 :: Pushy2 m vec a
    => (Data Length -> Data Length -> (Data Index, Data Index) -> (Data Index, Data Index))
    -> vec ->  Push2 m a
forwardPermute2 p vec = Push2 r c $ \write ->
    dumpPush2 v $ \i j a -> do
      let (i',j') = p r c (i,j)
      write i' j' a
  where
    v     = toPush2 vec
    (r,c) = extent2 v

transposePush :: Pushy2 m vec a => vec -> Push2 m a
transposePush vec = Push2 c r $ \write ->
    dumpPush2 v $ \i j a ->
      write j i a
  where
    v     = toPush2 vec
    (r,c) = extent2 v



--------------------------------------------------------------------------------
-- * Sequential vectors
--------------------------------------------------------------------------------

-- | Finite sequential vector
--
-- Users interested in infinite streams are referred to the library:
-- <https://github.com/emilaxelsson/feldspar-synch>
data Seq m a
  where
    Seq :: Data Length -> m (Data Index -> m a) -> Seq m a

-- | 'Seq' vector specialized to 'Data' elements
type DSeq m a = Seq m (Data a)

instance Monad m => Functor (Seq m)
  where
    fmap f (Seq len init) = Seq len $ do
      next <- init
      return $ fmap f . next

instance Finite (Seq m a)
  where
    length (Seq len _) = len

instance
    ( Syntax a
    , MarshalHaskell (Internal a)
    , MarshalFeld a
    , m ~ Run
    ) =>
      MarshalFeld (Seq m a)
  where
    type HaskellRep (Seq m a) = HaskellRep (Manifest a)

    fwrite hdl (Seq len init) = do
      next <- init
      fwrite hdl len >> printf " "
      for (0,1,Excl len) $ \i -> next i >>= fwrite hdl >> printf " "

    fread hdl = toSeq . (id :: Manifest _ -> _) <$> fread hdl
      -- Need to go through a temporary array to avoid embedding side-effects in
      -- the resulting vector. E.g. we don't want to duplicate the reads if the
      -- vector is duplicated.

-- | Vectors that can be converted to 'Seq'
class Seqy m vec a | vec -> a
  where
    -- | Convert a vector to 'Seq'
    toSeq :: vec -> Seq m a

-- | A version of 'toSeq' that constrains the @m@ argument of 'Seq' to that of
-- the monad in which the result is returned. This can be a convenient way to
-- avoid unresolved overloading.
toSeqM :: (Seqy m vec a, Monad m) => vec -> m (Seq m a)
toSeqM = return . toSeq

instance (Syntax a, MonadComp m) => Seqy m (Manifest a) a where toSeq = toSeq . toPull
instance (m1 ~ m2)               => Seqy m1 (Seq m2 a) a  where toSeq = id

instance MonadComp m => Seqy m (Pull a) a
  where
    toSeq vec = Seq (length vec) $ return $ \i -> return $ vec!i

zipWithSeq :: Monad m => (a -> b -> c) -> Seq m a -> Seq m b -> Seq m c
zipWithSeq f (Seq l1 init1) (Seq l2 init2) = Seq (min l1 l2) $ do
    next1 <- init1
    next2 <- init2
    return $ \i -> f <$> next1 i <*> next2 i

unfold :: (Syntax b, MonadComp m) => Data Length -> (b -> (b,a)) -> b -> Seq m a
unfold len step init = Seq len $ do
    r <- initRef init
    return $ \_ -> do
      acc <- getRef r
      let (acc',a) = step acc
      setRef r acc'
      return a

scan :: (Seqy m vec b, Syntax a, MonadComp m) =>
    (a -> b -> a) -> a -> vec -> Seq m a
scan step a vec = Seq (len+1) $ do
    next <- init
    r    <- initRef a
    return $ \i -> do
      a   <- next i
      acc <- getRef r
      setRef r $ step acc a
      return acc
  where
    Seq len init = toSeq vec

scan1 :: (Seqy m vec a, Syntax a, MonadComp m) =>
    (a -> a -> a) -> vec -> Seq m a
scan1 step vec = Seq len $ do
    assertLabel InternalAssertion (len > 0) "scan1: empty vector"
    next <- init
    a    <- next 0
    r    <- initRef a
    return $ \i -> do
      a <- getRef r
      b <- next (i + 1)
      let c = step a b
      setRef r c
      return c
  where
    Seq len init = toSeq vec

mapAccum :: (Seqy m vec a, Syntax acc, MonadComp m) =>
    (acc -> a -> (acc,b)) -> acc -> vec -> Seq m b
mapAccum step acc0 vec = Seq len $ do
    next <- init
    r    <- initRef acc0
    return $ \i -> do
      a   <- next i
      acc <- getRef r
      let (acc',b) = step acc a
      setRef r acc'
      return b
  where
    Seq len init = toSeq vec



--------------------------------------------------------------------------------
-- * Writing to memory
--------------------------------------------------------------------------------

-- It would be possible to make the `vec` parameter to `ViewManifest` and
-- `Manifestable` have kind `* -> *` and avoid the `a` parameter. But the
-- current design was chosen for consistency with `ViewManifest2` and
-- `Manifestable2`.

class ViewManifest vec a | vec -> a
  where
    -- | Try to cast a vector to 'Manifest' directly
    viewManifest :: vec -> Maybe (Manifest a)
    viewManifest _ = Nothing

instance ViewManifest (Manifest a) a where viewManifest = Just
instance ViewManifest (Pull a) a
instance ViewManifest (Push m a) a
instance ViewManifest (Seq m a) a

class ViewManifest vec a => Manifestable m vec a | vec -> a
  where
    -- | Write the contents of a vector to memory and get it back as a
    -- 'Manifest' vector. The supplied array may or may not be used for storage.
    manifest :: Syntax a
        => Arr a  -- ^ Where to store the vector
        -> vec    -- ^ Vector to store
        -> m (Manifest a)

    default manifest :: (Pushy m vec a, Finite vec, Syntax a, MonadComp m) =>
        Arr a -> vec -> m (Manifest a)
    manifest loc vec = do
        dumpPush v $ \i a -> setArr loc i a
        unsafeFreezeSlice (length vec) loc
      where
        v = toPush vec

    -- | A version of 'manifest' that allocates a fresh array for the result
    manifestFresh :: Syntax a => vec -> m (Manifest a)

    default manifestFresh :: (Finite vec, Syntax a, MonadComp m) =>
        vec -> m (Manifest a)
    manifestFresh vec = do
        loc <- newArr $ length vec
        manifest loc vec

    -- | A version of 'manifest' that only stores the vector to the given array
    -- ('manifest' is not guaranteed to use the array)
    manifestStore :: Syntax a => Arr a -> vec -> m ()

    default manifestStore :: (Pushy m vec a, Syntax a, MonadComp m) =>
        Arr a -> vec -> m ()
    manifestStore loc = void . manifest loc . toPush

-- | 'manifest' and 'manifestFresh' are no-ops. 'manifestStore' does a proper
-- 'arrCopy'.
instance MonadComp m => Manifestable m (Manifest a) a
  where
    manifest _        = return
    manifestFresh     = return
    manifestStore loc = copyArr loc <=< unsafeThawArr

instance MonadComp m             => Manifestable m (Pull a) a
instance (MonadComp m1, m1 ~ m2) => Manifestable m1 (Push m2 a) a
instance (MonadComp m1, m1 ~ m2) => Manifestable m1 (Seq m2 a) a


class ViewManifest2 vec a | vec -> a
  where
    -- | Try to cast a vector to 'Manifest2' directly
    viewManifest2 :: vec -> Maybe (Manifest2 a)
    viewManifest2 _ = Nothing

instance ViewManifest2 (Manifest2 a) a where viewManifest2 = Just
instance ViewManifest2 (Pull2 a) a
instance ViewManifest2 (Push2 m a) a

class ViewManifest2 vec a => Manifestable2 m vec a | vec -> a
  where
    -- | Write the contents of a vector to memory and get it back as a
    -- 'Manifest2' vector
    manifest2 :: Syntax a
        => Arr a  -- ^ Where to store the result
        -> vec    -- ^ Vector to store
        -> m (Manifest2 a)

    default manifest2 :: (Pushy2 m vec a, Syntax a, MonadComp m) =>
        Arr a -> vec -> m (Manifest2 a)
    manifest2 loc vec = do
        dumpPush2 v $ \i j a -> setArr loc (i*c + j) a
        nest r c <$> unsafeFreezeSlice (r*c) loc
      where
        v     = toPush2 vec
        (r,c) = extent2 v

    -- | A version of 'manifest2' that allocates a fresh array for the result
    manifestFresh2 :: Syntax a => vec -> m (Manifest2 a)

    default manifestFresh2 :: (Finite2 vec, Syntax a, MonadComp m) =>
        vec -> m (Manifest2 a)
    manifestFresh2 vec = do
        loc <- newArr (numRows vec * numCols vec)
        manifest2 loc vec

    -- | A version of 'manifest2' that only stores the vector to the given array
    -- ('manifest2' is not guaranteed to use the array)
    manifestStore2 :: Syntax a => Arr a -> vec -> m ()

    default manifestStore2 :: (Pushy2 m vec a, Syntax a, MonadComp m) =>
        Arr a -> vec -> m ()
    manifestStore2 loc = void . manifest2 loc . toPush2

-- | 'manifest2' and 'manifestFresh2' are no-ops. 'manifestStore2' does a proper
-- 'arrCopy'.
instance MonadComp m => Manifestable2 m (Manifest2 a) a
  where
    manifest2 _        = return
    manifestFresh2     = return
    manifestStore2 loc = copyArr loc <=< unsafeThawArr . unnest

instance MonadComp m             => Manifestable2 m (Pull2 a) a
instance (MonadComp m1, m1 ~ m2) => Manifestable2 m1 (Push2 m2 a) a

