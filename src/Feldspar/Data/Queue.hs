-- Copyright (c) 2016, Emil Axelsson, Peter Jonsson, Anders Persson and
--                     Josef Svenningsson
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | Indexable FIFO queues

module Feldspar.Data.Queue
  ( Queue (..)
  , initQueueFromBuffer
  , initQueue
  , newQueue
  , initQueueFromBuffer2
  , initQueue2
  , newQueue2
  ) where



import Prelude ()

import Feldspar
import Feldspar.Data.Vector



-- | Indexable FIFO queue
data Queue a = Queue
    { indexQ :: forall m   . MonadComp m => Data Index -> m a
    , putQ   :: forall m   . MonadComp m => a -> m ()
    , withQ  :: forall m b . (Syntax b, MonadComp m) => (Pull a -> m b) -> m b
    }

-- Another option would be to represent a queue as its state (the counter and
-- the array), but the above representation leaves room for other
-- implementations.

-- | Create a new cyclic queue using an existing array as buffer. The length of
-- the array determines the queue size.
initQueueFromBuffer :: forall m a . (Syntax a, MonadComp m) =>
    Arr a -> m (Queue a)
initQueueFromBuffer buf = do
    ir <- initRef 0
    let indexQ :: forall m2 . MonadComp m2 => Data Index -> m2 a
        indexQ j = do
          i <- unsafeFreezeRef ir
          getArr buf $ calcIndex i j
        putQ :: forall m2 . MonadComp m2 => a -> m2 ()
        putQ a = do
          i <- unsafeFreezeRef ir
          setArr buf i a
          setRef ir ((i+1) `mod` len)
        withQ :: forall m2 b . (Syntax b, MonadComp m2) => (Pull a -> m2 b) -> m2 b
        withQ f = do
          i   <- unsafeFreezeRef ir
          vec <- unsafeFreezeArr buf
          f $ backPermute (\_ -> calcIndex i) vec
    return Queue {..}
  where
    len = length buf
    calcIndex i j = (len+i-j-1) `mod` len

-- | Create a new cyclic queue initialized by the given vector (which also
-- determines the size)
initQueue :: (Manifestable m vec a, Finite vec, Syntax a, MonadComp m)
    => vec  -- ^ Initial content (also determines the queue size)
    -> m (Queue a)
initQueue init = do
    buf <- newArr $ length init
    manifestStore buf init
    initQueueFromBuffer buf

-- | Create a new cyclic queue of the given length without initialization
newQueue :: (Syntax a, MonadComp m) => Data Length -> m (Queue a)
newQueue l = newArr l >>= initQueueFromBuffer

initQueueFromBuffer2 :: forall m a . (Syntax a, MonadComp m)
    => Data Length  -- ^ Queue size, must be <= half the buffer size
    -> Arr a        -- ^ Buffer
    -> m (Queue a)
initQueueFromBuffer2 len buf = do
    ir <- initRef 0
    let indexQ :: forall m2 . MonadComp m2 => Data Index -> m2 a
        indexQ j = do
          i <- unsafeFreezeRef ir
          getArr buf (len+i-j-1)
        putQ :: forall m2 . MonadComp m2 => a -> m2 ()
        putQ a = do
          i <- unsafeFreezeRef ir
          setArr buf i a
          setArr buf (i+len) a
          setRef ir ((i+1) `mod` len)
        withQ :: forall m2 b . (Syntax b, MonadComp m2) => (Pull a -> m2 b) -> m2 b
        withQ f = do
          i <- unsafeFreezeRef ir
          vec <- unsafeFreezeArr buf
          f $ reverse $ take len $ drop i vec
    return Queue {..}

-- | Create a new cyclic queue. This implementation uses a buffer twice as long
-- as the queue size to avoid modulus operations when accessing the elements.
initQueue2 :: (Pushy m vec a, Finite vec, Syntax a, MonadComp m)
    => vec  -- ^ Initial content (also determines the queue size)
    -> m (Queue a)
initQueue2 init = do
    buf <- newArr (2*len)
    manifestStore buf (init++init)
    initQueueFromBuffer2 len buf
  where
    len = length init

-- | Create a new cyclic queue. This implementation uses a buffer twice as long
-- as the queue size to avoid modulus operations when accessing the elements.
newQueue2 :: (Syntax a, MonadComp m)
    => Data Length  -- ^ Queue size
    -> m (Queue a)
newQueue2 l = do
    buf <- newArr (2*l)
    initQueueFromBuffer2 l buf

