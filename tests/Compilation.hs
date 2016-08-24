{-# LANGUAGE ScopedTypeVariables #-}

import qualified Prelude
import Control.Monad.Trans

import Feldspar.Run
import Feldspar.Data.Vector
import Feldspar.Data.Option



--------------------------------------------------------------------------------
-- * Option
--------------------------------------------------------------------------------

-- | Safe indexing in a 'Manifest' vector
indexO :: (Syntax a, Monad m) => Manifest a -> Data Index -> OptionT m a
indexO vec i = guarded "indexO: out of bounds" (i<length vec) (vec!i)

funO :: Monad m => Manifest (Data Int32) -> Data Index -> OptionT m (Data Int32)
funO vec i = do
    a <- indexO vec i
    b <- indexO vec (i+1)
    c <- indexO vec (i+2)
    d <- indexO vec (i+4)
    return (a+b+c+d)

test_option :: Run ()
test_option = do
    vec <- manifestFresh $ fmap i2n (1...10)
    i   <- readStd
    printf "%d\n" $ fromSome $ funO vec i

test_optionM :: Run ()
test_optionM = do
    vec <- manifestFresh $ fmap i2n (1...10)
    i   <- readStd
    caseOptionM (funO vec i)
        printf
        (printf "%d\n")

readPositive :: OptionT Run (Data Int32)
readPositive = do
    i <- lift $ readStd
    guarded "negative" (i>=0) (i :: Data Int32)

test_optionT = optionT printf (\_ -> return ()) $ do
    vec  <- manifestFresh $ fmap i2n (1...10)
    len  <- readPositive
    sumr <- initRef (0 :: Data Int32)
    for (0, 1, Excl len) $ \i -> do
        lift $ printf "reading index %d\n" i
        x <- indexO vec (i2n i)
        modifyRefD sumr (+x)
    s <- unsafeFreezeRef sumr
    lift $ printf "%d" (s :: Data Int32)



--------------------------------------------------------------------------------
-- * Misc.
--------------------------------------------------------------------------------

-- Test that constant folding does not attempt to fold array indexing
test_constFoldArr :: Run ()
test_constFoldArr = do
    arr <- initIArr [1..10]
    let a :: Data Int32 = (arrIx arr 0 == arrIx arr 1) ? arrIx arr 100 $ arrIx arr 2
    printf "%d\n" a



--------------------------------------------------------------------------------

main = do
    tag "test_option"       >> compareCompiled test_option  (runIO test_option)  "5\n"
    tag "test_option"       >> compareCompiled test_option  (runIO test_option)  "6\n"
    tag "test_optionM"      >> compareCompiled test_optionM (runIO test_option)  "5\n"
    tag "test_optionM"      >> compareCompiled test_optionM (runIO test_optionM) "6\n"
    tag "test_optionT"      >> compareCompiled test_optionT (runIO test_optionT) "10\n"
    tag "test_constFoldArr" >> compareCompiled test_constFoldArr (runIO test_constFoldArr) ""
  where
    tag str = putStrLn $ "---------------- examples/Demo.hs/" Prelude.++ str Prelude.++ "\n"

