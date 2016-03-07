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



import Language.Embedded.Concurrent (ThreadId, ChanBound, Chan, Closeable, Uncloseable)
import qualified Language.Embedded.Concurrent as Imp

import Feldspar.Representation
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

-- | Create a new channel. Writing a reference type to a channel will copy the
--   /reference/ into the queue, not its contents.
--
--   We'll likely want to change this, actually copying arrays and the like
--   into the queue instead of sharing them across threads.
newChan :: SmallType a => Data ChanBound -> Run (Chan Uncloseable a)
newChan = Run . Imp.newChan

newCloseableChan :: SmallType a => Data ChanBound -> Run (Chan Closeable a)
newCloseableChan = Run . Imp.newCloseableChan

-- | Read an element from a channel. If channel is empty, blocks until there
--   is an item available.
--   If 'closeChan' has been called on the channel *and* if the channel is
--   empty, @readChan@ returns an undefined value immediately.
readChan :: SmallType a => Chan t a -> Run (Data a)
readChan = Run . Imp.readChan

-- | Write a data element to a channel.
--   If 'closeChan' has been called on the channel, all calls to @writeChan@
--   become non-blocking no-ops and return @False@, otherwise returns @True@.
writeChan :: SmallType a => Chan t a -> Data a -> Run (Data Bool)
writeChan c = Run . Imp.writeChan c

-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: SmallType a => Chan Closeable a -> Run (Data Bool)
lastChanReadOK = Run . Imp.lastChanReadOK

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: SmallType a => Chan Closeable a -> Run ()
closeChan = Run . Imp.closeChan

