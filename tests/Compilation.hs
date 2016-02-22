{-# LANGUAGE ScopedTypeVariables #-}

import qualified Prelude
import Control.Monad.Trans

import Feldspar.Software
import Feldspar.Vector
import Feldspar.Option



--------------------------------------------------------------------------------
-- * Option
--------------------------------------------------------------------------------

-- | Safe indexing in a 'Manifest' vector
indexO :: (Syntax a, Monad m) => Manifest a -> Data Index -> OptionT m a
indexO (Manifest len arr) i =
    guarded "indexO: out of bounds" (i<len) (arrIx arr i)

funO :: Monad m => Manifest (Data Int32) -> Data Index -> OptionT m (Data Int32)
funO vec i = do
    a <- indexO vec i
    b <- indexO vec (i+1)
    c <- indexO vec (i+2)
    d <- indexO vec (i+4)
    return (a+b+c+d)

test_option :: Software ()
test_option = do
    vec <- fromPull $ map i2n (1...10)
    i   <- fget stdin
    printf "%d\n" $ fromSome $ funO vec i

test_optionM :: Software ()
test_optionM = do
    vec <- fromPull $ map i2n (1...10)
    i <- fget stdin
    caseOptionM (funO vec i)
        printf
        (printf "%d\n")

readPositive :: OptionT Software (Data Int32)
readPositive = do
    i <- lift $ fget stdin
    guarded "negative" (i>=0) (i :: Data Int32)

test_optionT = optionT printf (\_ -> return ()) $ do
    vec <- fromPull $ map i2n (1...10)
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
test_constFoldArr :: Software ()
test_constFoldArr = do
    arr <- initIArr [1..10]
    let a :: Data Int32 = (arrIx arr 0 == arrIx arr 1) ? arrIx arr 100 $ arrIx arr 2
    printf "%d\n" a



--------------------------------------------------------------------------------

main = do
    compareCompiled test_option  (runIO test_option)  "5\n"
    compareCompiled test_option  (runIO test_option)  "6\n"
    compareCompiled test_optionM (runIO test_option)  "5\n"
    compareCompiled test_optionM (runIO test_optionM) "6\n"
    compareCompiled test_optionT (runIO test_optionT) "10\n"

    compareCompiled test_constFoldArr (runIO test_constFoldArr) ""

