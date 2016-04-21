module Feldspar.Run.Concurrent
  ( ThreadId
  , ChanBound
  , Chan
  , Closeable
  , Uncloseable
  , fork
  , forkWithId
  , asyncKillThread
  , killThread
  , waitThread
  , newChan
  , newCloseableChan
  , readChan
  , writeChan
  , closeChan
  , lastChanReadOK
  ) where



import Prelude hiding ((&&), all)
import Data.TypedStruct

import Language.Embedded.Concurrent (ThreadId, ChanBound, Closeable, Uncloseable)
import qualified Language.Embedded.Concurrent as Imp
import Language.Syntactic

import Feldspar (true, (&&))
import Feldspar.Representation
import Feldspar.Primitive.Representation
import Feldspar.Run.Representation



-- | Fork off a computation as a new thread.
fork :: Run () -> Run ThreadId
fork = Run . Imp.fork . unRun

-- | Fork off a computation as a new thread, with access to its own thread ID.
forkWithId :: (ThreadId -> Run ()) -> Run ThreadId
forkWithId f = Run $ Imp.forkWithId (unRun . f)

-- | Forcibly terminate a thread, then continue execution immediately.
asyncKillThread :: ThreadId -> Run ()
asyncKillThread = Run . Imp.asyncKillThread

-- | Forcibly terminate a thread. Blocks until the thread is actually dead.
killThread :: ThreadId -> Run ()
killThread = Run . Imp.killThread

-- | Wait for a thread to terminate.
waitThread :: ThreadId -> Run ()
waitThread = Run . Imp.waitThread


-- | Communication channel
newtype Chan b a = Chan { unChan :: Struct PrimType' (Imp.Chan b) a }

-- | Create a new channel. Writing a reference type to a channel will copy the
--   /reference/ into the queue, not its contents.
--
--   We'll likely want to change this, actually copying arrays and the like
--   into the queue instead of sharing them across threads.
newChan :: Type a => Data ChanBound -> Run (Chan Uncloseable a)
newChan b = Chan <$> mapStructA (const $ Run $ Imp.newChan b) typeRep

newCloseableChan :: Type a => Data ChanBound -> Run (Chan Closeable a)
newCloseableChan b = Chan <$> mapStructA (const $ Run $ Imp.newCloseableChan b) typeRep

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
--   If 'closeChan' has been called on the channel *and* if the channel is
--   empty, @readChan@ returns an undefined value immediately.
readChan :: Syntax a => Chan t (Internal a) -> Run a
readChan = fmap resugar . mapStructA (Run . Imp.readChan) . unChan

-- | Write a data element to a channel.
--   If 'closeChan' has been called on the channel, all calls to @writeChan@
--   become non-blocking no-ops and return @False@, otherwise returns @True@.
writeChan :: Syntax a => Chan t (Internal a) -> a -> Run (Data Bool)
writeChan c
    = all
    . zipListStruct (\c' a' -> Run $ Imp.writeChan c' a') (unChan c)
    . resugar

-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: Type a => Chan Closeable a -> Run(Data Bool)
lastChanReadOK = all . listStruct (Run . Imp.lastChanReadOK) . unChan

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: Type a => Chan Closeable a -> Run ()
closeChan = mapStructA_ (Run . Imp.closeChan) . unChan


-- | Conjunction of boolean computation results
all :: [Run (Data Bool)] -> Run (Data Bool)
all = foldl (\a b -> do { x <- a; y <- b; return (x && y) }) (return true)
