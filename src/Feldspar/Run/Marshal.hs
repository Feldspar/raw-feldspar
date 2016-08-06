{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Run.Marshal where



import qualified Prelude
import Prelude hiding (length)

import Data.Typeable

import Feldspar
import Feldspar.Run.Representation
import Feldspar.Run.Compile
import Feldspar.Run.Frontend

import Language.Embedded.Backend.C (ExternalCompilerOpts (..), Default (..))



newtype Parser a = Parser {runParser :: String -> (a, String)}
  deriving (Functor)

instance Applicative Parser
  where
    pure  = return
    (<*>) = ap

instance Monad Parser
  where
    return a = Parser $ \s -> (a,s)
    p >>= k  = Parser $ \s -> let (a,s') = runParser p s in runParser (k a) s'

readParser :: forall a . (Read a, Typeable a) => Parser a
readParser = Parser $ \s -> case reads s of
    [(a,s')] -> (a,s')
    _        -> error $ unwords
        [ "toHaskell: cannot read"
        , show s
        , "as type"
        ,  show (typeOf (undefined :: a))
        ]

parse :: Parser a -> String -> a
parse = (fst .) . runParser

-- | Serialization/deserialization of Haskell values
--
-- The following property must hold for all @a@:
--
-- > a = parse toHaskell (fromHaskell a) Prelude.== a
class MarshalHaskell a
  where
    -- | Serialize a Haskell value
    fromHaskell :: a -> String
    default fromHaskell :: Show a => a -> String
    fromHaskell = show

    -- | Deserialize a Haskell value
    toHaskell :: Parser a
    default toHaskell :: (Read a, Typeable a) => Parser a
    toHaskell = readParser

instance MarshalHaskell Int
instance MarshalHaskell Int8
instance MarshalHaskell Int16
instance MarshalHaskell Int32
instance MarshalHaskell Int64
instance MarshalHaskell Word8
instance MarshalHaskell Word16
instance MarshalHaskell Word32
instance MarshalHaskell Word64
instance MarshalHaskell Float
instance MarshalHaskell Double

instance MarshalHaskell (Complex Float)
  where
    fromHaskell (r :+ i) = fromHaskell (r,i)
    toHaskell = fmap (uncurry (:+)) toHaskell

instance MarshalHaskell (Complex Double)
  where
    fromHaskell (r :+ i) = fromHaskell (r,i)
    toHaskell = fmap (uncurry (:+)) toHaskell

instance (MarshalHaskell a, MarshalHaskell b) => MarshalHaskell (a,b)
  where
    fromHaskell (a,b) = unwords [fromHaskell a, fromHaskell b]
    toHaskell = (,) <$> toHaskell <*> toHaskell

instance (MarshalHaskell a, MarshalHaskell b, MarshalHaskell c) => MarshalHaskell (a,b,c)
  where
    fromHaskell (a,b,c) = unwords [fromHaskell a, fromHaskell b, fromHaskell c]
    toHaskell = (,,) <$> toHaskell <*> toHaskell <*> toHaskell

instance (MarshalHaskell a, MarshalHaskell b, MarshalHaskell c, MarshalHaskell d) => MarshalHaskell (a,b,c,d)
  where
    fromHaskell (a,b,c,d) = unwords [fromHaskell a, fromHaskell b, fromHaskell c, fromHaskell d]
    toHaskell = (,,,) <$> toHaskell <*> toHaskell <*> toHaskell <*> toHaskell

instance MarshalHaskell a => MarshalHaskell [a]
  where
    fromHaskell as = unwords $ show (Prelude.length as) : map fromHaskell as

    toHaskell = do
        len <- toHaskell
        replicateM len toHaskell

-- | Serialization/deserialization of Feldspar values
class (MarshalHaskell (HaskellRep a)) => MarshalFeld a
  where
    -- | The Haskell representation of a Feldspar value
    type HaskellRep a

    -- | Serialize a Feldspar value to @stdout@
    fromFeld :: a -> Run ()
    default fromFeld :: (PrimType b, Formattable b, a ~ Data b) => a -> Run ()
    fromFeld i = fput stdout "" i ""

    -- | Deserialize a Feldspar value from @stdin@
    toFeld :: Run a
    default toFeld :: (PrimType b, Formattable b, a ~ Data b) => Run a
    toFeld = fget stdin

instance MarshalFeld (Data Int8)   where type HaskellRep (Data Int8)   = Int8
instance MarshalFeld (Data Int16)  where type HaskellRep (Data Int16)  = Int16
instance MarshalFeld (Data Int32)  where type HaskellRep (Data Int32)  = Int32
instance MarshalFeld (Data Int64)  where type HaskellRep (Data Int64)  = Int64
instance MarshalFeld (Data Word8)  where type HaskellRep (Data Word8)  = Word8
instance MarshalFeld (Data Word16) where type HaskellRep (Data Word16) = Word16
instance MarshalFeld (Data Word32) where type HaskellRep (Data Word32) = Word32
instance MarshalFeld (Data Word64) where type HaskellRep (Data Word64) = Word64
instance MarshalFeld (Data Float)  where type HaskellRep (Data Float)  = Float
instance MarshalFeld (Data Double) where type HaskellRep (Data Double) = Double

instance MarshalFeld (Data (Complex Float))
  where
    type HaskellRep (Data (Complex Float)) = Complex Float
    fromFeld c = fromFeld (realPart c, imagPart c)
    toFeld = fmap (uncurry complex) toFeld

instance MarshalFeld (Data (Complex Double))
  where
    type HaskellRep (Data (Complex Double)) = Complex Double
    fromFeld c = fromFeld (realPart c, imagPart c)
    toFeld = fmap (uncurry complex) toFeld

instance (MarshalFeld a, MarshalFeld b) => MarshalFeld (a,b)
  where
    type HaskellRep (a,b) = (HaskellRep a, HaskellRep b)
    fromFeld (a,b) = fromFeld a >> printf " " >> fromFeld b
    toFeld = (,) <$> toFeld <*> toFeld

instance (MarshalFeld a, MarshalFeld b, MarshalFeld c) => MarshalFeld (a,b,c)
  where
    type HaskellRep (a,b,c) = (HaskellRep a, HaskellRep b, HaskellRep c)
    fromFeld (a,b,c) = fromFeld a >> printf " " >> fromFeld b >> printf " " >> fromFeld c
    toFeld = (,,) <$> toFeld <*> toFeld <*> toFeld

instance (MarshalFeld a, MarshalFeld b, MarshalFeld c, MarshalFeld d) => MarshalFeld (a,b,c,d)
  where
    type HaskellRep (a,b,c,d) = (HaskellRep a, HaskellRep b, HaskellRep c, HaskellRep d)
    fromFeld (a,b,c,d) = fromFeld a >> printf " " >> fromFeld b >> printf " " >> fromFeld c >> printf " " >> fromFeld d
    toFeld = (,,,) <$> toFeld <*> toFeld <*> toFeld <*> toFeld

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) => MarshalFeld (Arr a)
  where
    type HaskellRep (Arr a) = [a]

    fromFeld arr = do
        len <- force $ length arr
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
        return arr

instance (MarshalHaskell a, MarshalFeld (Data a), Type a) =>
    MarshalFeld (IArr a)
  where
    type HaskellRep (IArr a) = [a]

    fromFeld arr = do
        len <- force $ length arr
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
        iarr <- freezeArr arr
        return iarr

-- | Connect a Feldspar function between serializable types to @stdin@/@stdout@
connectStdIO :: (MarshalFeld a, MarshalFeld b) => (a -> Run b) -> Run ()
connectStdIO f = (toFeld >>= f) >>= fromFeld

-- | A version of 'marshalled' that takes 'ExternalCompilerOpts' as additional
-- argument
marshalled' :: (MarshalFeld a, MarshalFeld b)
    => CompilerOpts
    -> ExternalCompilerOpts
    -> (a -> Run b)  -- ^ Function to compile
    -> ((HaskellRep a -> IO (HaskellRep b)) -> IO c)
         -- ^ Function that has access to the compiled executable as a function
    -> IO c
marshalled' opts eopts f body =
    withCompiled' opts eopts (connectStdIO f) $ \g ->
      body (fmap (parse toHaskell) . g . fromHaskell)

-- | Compile a function and make it available as an 'IO' function. Note that
-- compilation only happens once, even if the function is used many times in the
-- body.
--
-- For example, given the following Feldspar function:
--
-- > sumArr :: Fin (IArr Int32) -> Run (Data Int32)
-- > sumArr (Fin l arr) = do
-- >     r <- initRef (0 :: Data Int32)
-- >     for (0,1,Excl l) $ \i -> modifyRefD r (+ arrIx arr i)
-- >     unsafeFreezeRef r
--
-- 'marshalled' can be used as follows:
--
-- > *Main> marshalled sumArr $ \f -> (f [3,4,5] >>= print) >> (f [6,7,8,9] >>= print)
marshalled :: (MarshalFeld a, MarshalFeld b)
    => (a -> Run b)  -- ^ Function to compile
    -> ((HaskellRep a -> IO (HaskellRep b)) -> IO c)
         -- ^ Function that has access to the compiled executable as a function
    -> IO c
marshalled = marshalled' def def

