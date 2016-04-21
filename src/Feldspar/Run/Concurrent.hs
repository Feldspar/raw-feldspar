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
  , readChan
  , writeChan
  , closeChan
  , lastChanReadOK
  ) where



import Data.Proxy

import Language.Embedded.Concurrent (ThreadId, ChanBound, Closeable, Uncloseable)
import qualified Language.Embedded.Concurrent as Imp

import Feldspar.Frontend
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



--------------------------------------------------------------------------------
-- * 'Transferable' class
--------------------------------------------------------------------------------

class Transferable a
  where
    -- | Channel data representation
    type ChanRep a

    -- | Create a new channel. Writing a reference type to a channel will copy the
    --   /reference/ into the queue, not its contents.
    --
    --   We'll likely want to change this, actually copying arrays and the like
    --   into the queue instead of sharing them across threads.
    newChanRep :: proxy a -> Data ChanBound -> Run (ChanRep a)

    -- | Read an element from a channel. If channel is empty, blocks until there
    --   is an item available.
    --   If 'closeChan' has been called on the channel *and* if the channel is
    --   empty, @readChan@ returns an undefined value immediately.
    readChanRep :: ChanRep a -> Run a

    -- | Write a data element to a channel.
    --   If 'closeChan' has been called on the channel, all calls to @writeChan@
    --   become non-blocking no-ops and return @False@, otherwise returns @True@.
    writeChanRep :: ChanRep a -> a -> Run (Data Bool)

    -- | When 'readChan' was last called on the given channel, did the read
    --   succeed?
    --   Always returns @True@ unless 'closeChan' has been called on the channel.
    --   Always returns @True@ if the channel has never been read.
    lastChanReadOKRep :: proxy a -> ChanRep a -> Run (Data Bool)

    -- | Close a channel. All subsequent write operations will be no-ops.
    --   After the channel is drained, all subsequent read operations will be
    --   no-ops as well.
    closeChanRep :: proxy a -> ChanRep a -> Run ()

instance PrimType a => Transferable (Data a)
  where
    type ChanRep (Data a) = Imp.Chan Closeable a
    newChanRep _        = Run . Imp.newCloseableChan
    readChanRep         = Run . Imp.readChan
    writeChanRep c      = Run . Imp.writeChan c
    lastChanReadOKRep _ = Run . Imp.lastChanReadOK
    closeChanRep _      = Run . Imp.closeChan

instance (Transferable a, Transferable b) => Transferable (a,b)
  where
    type ChanRep (a,b) = (ChanRep a, ChanRep b)
    newChanRep _ sz = (,) <$> newChanRep (Proxy :: Proxy a) sz <*> newChanRep (Proxy :: Proxy b) sz
    readChanRep (a,b) = (,) <$> readChanRep a <*> readChanRep b
    writeChanRep (a,b) (va,vb) = do
        sa <- writeChanRep a va
        ifE sa (writeChanRep b vb) (return false)
    lastChanReadOKRep _ (a,b) = do
        sa <- lastChanReadOKRep (Proxy :: Proxy a) a
        ifE sa (lastChanReadOKRep (Proxy :: Proxy b) b) (return false)
    closeChanRep _ (a,b) = do
        closeChanRep (Proxy :: Proxy a) a
        closeChanRep (Proxy :: Proxy b) b



--------------------------------------------------------------------------------
-- * User interface for channels
--------------------------------------------------------------------------------

-- | Communication channel
newtype Chan a = Chan { unChan :: ChanRep a }

newChan :: forall a. Transferable a => Data ChanBound -> Run (Chan a)
newChan = fmap Chan . newChanRep (Proxy :: Proxy a)

readChan :: Transferable a => Chan a -> Run a
readChan = readChanRep . unChan

writeChan :: Transferable a => Chan a -> a -> Run (Data Bool)
writeChan c = writeChanRep (unChan c)

lastChanReadOK :: Transferable a => Chan a -> Run (Data Bool)
lastChanReadOK c = lastChanReadOKRep c (unChan c)

closeChan :: Transferable a => Chan a -> Run ()
closeChan c = closeChanRep c (unChan c)
