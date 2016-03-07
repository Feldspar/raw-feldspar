module Concurrent where



import qualified Prelude

import Feldspar.Run
import Feldspar.Run.Concurrent



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



----------------------------------------

testAll = do
    tag "waiting" >> compareCompiled' opts waiting (runIO waiting)  ""
    tag "suicide" >> compareCompiled' opts suicide (runIO suicide)  ""
  where
    tag str = putStrLn $ "---------------- examples/Concurrent.hs/" Prelude.++ str Prelude.++ "\n"
    opts = defaultExtCompilerOpts {externalFlagsPost = ["-lpthread"]}

