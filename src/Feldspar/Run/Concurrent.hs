module Feldspar.Run.Concurrent
  ( ThreadId
  , Chan, Closeable, Uncloseable
  , Transferable (..), BulkTransferable (..)
  , fork
  , forkWithId
  , asyncKillThread
  , killThread
  , waitThread
  , closeChan
  , lastChanReadOK
  ) where



import Prelude hiding ((&&))
import Data.Proxy
import Data.TypedStruct

import qualified Language.Embedded.Concurrent as Imp
import Language.Embedded.Concurrent (ThreadId, Chan, Closeable, Uncloseable)

import Feldspar
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
-- * 'Transferable' classes
--------------------------------------------------------------------------------

class Transferable a
  where
    -- | Size specification of a channel. In most of the cases, it is a natural
    --   number representing how many elements could be stored at the same time
    --   in the channel.
    type SizeSpec a :: *

    -- | Maps a size specification to an internal channel size representation,
    --   that is a map from primitive types to quantities. The byte size of the
    --   channel will be calculated as the sum of multiplying the byte size of
    --   each type with its quantity.
    calcChanSize :: proxy a -> SizeSpec a -> Imp.ChanSize Data PrimType' Length

    -- | Create a new channel. Writing a reference type to a channel will copy
    --   contents into the channel, so modifying it post-write is completely
    --   safe.
    newChan :: SizeSpec a -> Run (Chan Uncloseable a)
    newChan = Run . Imp.newChan' . calcChanSize (Proxy :: Proxy a)

    newCloseableChan :: SizeSpec a -> Run (Chan Closeable a)
    newCloseableChan = Run . Imp.newCloseableChan' . calcChanSize (Proxy :: Proxy a)

    -- | Read an element from a channel. If channel is empty, blocks until there
    --   is an item available.
    --   If 'closeChan' has been called on the channel *and* if the channel is
    --   empty, @readChan@ returns an undefined value immediately.
    readChan :: Chan t a -> Run a
    readChan = untypedReadChan

    -- | Reads a value from any kind of channel. Instances should define this,
    --   but the user should never call it.
    untypedReadChan :: Chan t c -> Run a

    -- | Write a data element to a channel.
    --   If 'closeChan' has been called on the channel, all calls to @writeChan@
    --   become non-blocking no-ops and return @False@, otherwise returns @True@.
    --   If the channel is full, this function blocks until there's space in the
    --   queue.
    writeChan :: Chan t a -> a -> Run (Data Bool)
    writeChan = untypedWriteChan

    -- | Writes a value to any kind of channel. Instances should define this,
    --   but the user should never call it.
    untypedWriteChan :: Chan t c -> a -> Run (Data Bool)


class Transferable a => BulkTransferable a
  where
    type ContainerType a :: *

    -- | Read an arbitrary number of elements from a channel into an array.
    --   The semantics are the same as for 'readChan', where "channel is empty"
    --   is defined as "channel contains less data than requested".
    --   Returns @False@ without reading any data if the channel is closed.
    readChanBuf :: Chan t a
                -> Data Index -- ^ Offset in array to start writing
                -> Data Index -- ^ Elements to read
                -> (ContainerType a)
                -> Run (Data Bool)
    readChanBuf = untypedReadChanBuf (Proxy :: Proxy a)

    -- | Read an arbitrary number of elements from any channel into an array.
    --   Instances should define this, but the user should never call it.
    untypedReadChanBuf :: proxy a
                       -> Chan t c
                       -> Data Index -- ^ Offset in array to start writing
                       -> Data Index -- ^ Elements to read
                       -> (ContainerType a)
                       -> Run (Data Bool)

    -- | Write an arbitrary number of elements from an array into an channel.
    --   The semantics are the same as for 'writeChan', where "channel is full"
    --   is defined as "channel has insufficient free space to store all written
    --   data".
    writeChanBuf :: Chan t a
                 -> Data Index -- ^ Offset in array to start reading
                 -> Data Index -- ^ Elements to write
                 -> (ContainerType a)
                 -> Run (Data Bool)
    writeChanBuf = untypedWriteChanBuf (Proxy :: Proxy a)

    -- | Write an arbitrary number of elements from an array into any channel.
    --   Instances should define this, but the user should never call it.
    untypedWriteChanBuf :: proxy a
                        -> Chan t c
                        -> Data Index -- ^ Offset in array to start reading
                        -> Data Index -- ^ Elements to write
                        -> (ContainerType a)
                        -> Run (Data Bool)

-- | When 'readChan' was last called on the given channel, did the read
--   succeed?
--   Always returns @True@ unless 'closeChan' has been called on the channel.
--   Always returns @True@ if the channel has never been read.
lastChanReadOK :: Chan Closeable a -> Run (Data Bool)
lastChanReadOK = Run . Imp.lastChanReadOK

-- | Close a channel. All subsequent write operations will be no-ops.
--   After the channel is drained, all subsequent read operations will be
--   no-ops as well.
closeChan :: Chan Closeable a -> Run ()
closeChan = Run . Imp.closeChan


--------------------------------------------------------------------------------
-- * 'Transferable' instances
--------------------------------------------------------------------------------

instance PrimType' a => Transferable (Data a)
  where
    type SizeSpec (Data a) = Data Length
    calcChanSize _ sz = sz `Imp.timesSizeOf` (Proxy :: Proxy a)
    untypedReadChan    = Run . Imp.readChan'
    untypedWriteChan c = Run . Imp.writeChan' c

instance PrimType' a => BulkTransferable (Data a)
  where
    type ContainerType (Data a) = Arr a
    untypedReadChanBuf  _ c off len arr = do
      r <- sequence $ listStruct (Run . Imp.readChanBuf' c off len) (unArr arr)
      return $ foldl1 (&&) r
    untypedWriteChanBuf _ c off len arr = do
      r <- sequence $ listStruct (Run . Imp.writeChanBuf' c off len) (unArr arr)
      return $ foldl1 (&&) r

instance ( Transferable a, Transferable b
         , SizeSpec a ~ SizeSpec b
         ) => Transferable (a, b)
  where
    type SizeSpec (a, b) = SizeSpec a
    calcChanSize _ sz =
      let asz = calcChanSize (Proxy :: Proxy a) sz
          bsz = calcChanSize (Proxy :: Proxy b) sz
      in  asz `Imp.plusSize` bsz
    untypedReadChan ch = (,) <$> untypedReadChan ch <*> untypedReadChan ch
    untypedWriteChan ch (a, b) = do
      sa <- untypedWriteChan ch a
      ifE sa (untypedWriteChan ch b) (return false)

instance ( Transferable a, Transferable b, Transferable c
         , SizeSpec a ~ SizeSpec b, SizeSpec b ~ SizeSpec c
         ) => Transferable (a, b, c)
  where
    type SizeSpec (a, b, c) = SizeSpec a
    calcChanSize _ sz =
      let asz = calcChanSize (Proxy :: Proxy a) sz
          bsz = calcChanSize (Proxy :: Proxy b) sz
          csz = calcChanSize (Proxy :: Proxy c) sz
      in  asz `Imp.plusSize` bsz `Imp.plusSize` csz
    untypedReadChan ch = (,,)
                     <$> untypedReadChan ch
                     <*> untypedReadChan ch
                     <*> untypedReadChan ch
    untypedWriteChan ch (a, b, c) = do
      sa <- untypedWriteChan ch a
      ifE sa
        (do sb <- untypedWriteChan ch b
            ifE sb (untypedWriteChan ch c) (return false))
        (return false)

instance ( Transferable a, Transferable b, Transferable c, Transferable d
         , SizeSpec a ~ SizeSpec b, SizeSpec b ~ SizeSpec c, SizeSpec c ~ SizeSpec d
         ) => Transferable (a, b, c, d)
  where
    type SizeSpec (a, b, c, d) = SizeSpec a
    calcChanSize _ sz =
      let asz = calcChanSize (Proxy :: Proxy a) sz
          bsz = calcChanSize (Proxy :: Proxy b) sz
          csz = calcChanSize (Proxy :: Proxy c) sz
          dsz = calcChanSize (Proxy :: Proxy d) sz
      in  asz `Imp.plusSize` bsz `Imp.plusSize` csz `Imp.plusSize` dsz
    untypedReadChan ch = (,,,)
                     <$> untypedReadChan ch
                     <*> untypedReadChan ch
                     <*> untypedReadChan ch
                     <*> untypedReadChan ch
    untypedWriteChan ch (a, b, c, d) = do
      sa <- untypedWriteChan ch a
      ifE sa
        (do sb <- untypedWriteChan ch b
            ifE sb
              (do sc <- untypedWriteChan ch c
                  ifE sc
                    (untypedWriteChan ch d)
                    (return false))
              (return false))
        (return false)
