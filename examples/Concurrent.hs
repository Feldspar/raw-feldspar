module Concurrent where



import qualified Prelude

import Feldspar.Run
import Feldspar.Run.Concurrent
import Feldspar.Vector

import Data.Proxy
import Data.TypedStruct


-- | Waiting for thread completion.
waiting :: Run ()
waiting = do
  t <- fork $ printf "Forked thread printing %d\n" (value 0 :: Data Int32)
  waitThread t
  printf "Main thread printing %d\n" (value 1 :: Data Int32)

-- | A thread kills itself using its own thread ID.
suicide :: Run ()
suicide = do
  tid <- forkWithId $ \tid -> do
    printf "This is printed. %d\n" (value 0 :: Data Int32)
    killThread tid
    printf "This is not. %d\n" (value 0 :: Data Int32)
  waitThread tid
  printf "The thread is dead, long live the thread! %d\n" (value 0 :: Data Int32)




primChan :: Run ()
primChan = do
    c :: Chan (Data Int32) <- newChan 10
    writer <- fork $ do
        printf "Writer started\n"
        writeChan c (42 :: Data Int32)
        printf "Writer ended\n"
    reader <- fork $ do
        printf "Reader started\n"
        v <- readChan c
        printf "Received: %d\n" v
    waitThread reader
    waitThread writer
    closeChan c

pairChan :: Run ()
pairChan = do
    c :: Chan (Data Int32, Data Word8) <- newChan 10
    writer <- fork $ do
        printf "Writer started\n"
        writeChan c (1337 :: Data Int32, 42 :: Data Word8)
        printf "Writer ended\n"
    reader <- fork $ do
        printf "Reader started\n"
        (a :: Data Int32, b :: Data Word8) <- readChan c
        printf "Received: (%d, %d)\n" a b
    waitThread reader
    waitThread writer
    closeChan c

vecChan :: Run ()
vecChan = do
    c :: Chan (Vector (Data Index)) <- newChan 1024
    writer <- fork $ do
        printf "Writer started\n"
        let v = map (+1) (0 ... 9)
        writeChan c v
        printf "Writer ended\n"
    reader <- fork $ do
        printf "Reader started\n"
        v <- readChan c
        for (0, 1, Excl 10) $ \i -> do
            printf "Received: (%d => %d)\n" i (v ! i)
    waitThread reader
    waitThread writer
    closeChan c


runStorableChanTest = mapM_ (runCompiled' opts) [ primChan, pairChan, vecChan ]
  where
    opts = defaultExtCompilerOpts
         { externalFlagsPost = ["-lpthread"]
         , externalFlagsPre  = [ "-I/home/km/Work/elte/phd/research/chalmers/git/imperative-edsl/include"
                               , "/home/km/Work/elte/phd/research/chalmers/git/imperative-edsl/csrc/chan.c" ] }



----------------------------------------

testAll = do
    tag "waiting" >> compareCompiled' opts waiting (runIO waiting)  ""
    tag "suicide" >> compareCompiled' opts suicide (runIO suicide)  ""
  where
    tag str = putStrLn $ "---------------- examples/Concurrent.hs/" Prelude.++ str Prelude.++ "\n"
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}
