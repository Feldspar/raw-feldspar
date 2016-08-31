module Feldspar.Processing.Filters where



import Prelude ()

import Feldspar
import Feldspar.Data.Vector
import Feldspar.Data.Queue



recurrenceI
    :: ( Pushy m fvec a
       , Finite fvec
       , Seqy m ivec a
       , Syntax a, Syntax b
       , MonadComp m
       )
    => fvec           -- ^ Initial input buffer
    -> ivec           -- ^ Input vector
    -> (Pull a -> b)  -- ^ Step function, producing one output from previous inputs
    -> Seq m b        -- ^ Output vector
recurrenceI ii vec body = Seq len $ do
    next <- init
    buf  <- initQueue2 ii
    return $ \i -> do
      a <- next i
      iff (length ii /= 0)
        (putQ buf a)
        (return ())
      withQ buf (return . body)
  where
    Seq len init = toSeq vec

recurrenceIO
    :: ( Pushy m fvec a
       , Finite fvec
       , Seqy m ivec a
       , Pushy m bvec b
       , Finite bvec
       , Syntax a, Syntax b
       , MonadComp m
       )
    => fvec                     -- ^ Initial input buffer
    -> ivec                     -- ^ Input vector
    -> bvec                     -- ^ Initial output buffer
    -> (Pull a -> Pull b -> b)  -- ^ Step function, producing one output from
                                --   previous inputs and outputs
    -> Seq m b                  -- ^ Output vector
recurrenceIO ii vec io body = Seq len $ do
    next <- init
    ibuf <- initQueue2 ii
    obuf <- initQueue2 io
    return $ \i -> do
      a <- next i
      iff (length ii /= 0)
        (putQ ibuf a)
        (return ())
      b <- withQ ibuf $ \ib ->
             withQ obuf $ \ob ->
               shareM $ body ib ob
                 -- Sharing important since `b` is shared
      iff (length io /= 0)
        (putQ obuf b)
        (return ())
      return b
  where
    Seq len init = toSeq vec

-- | FIR filter
fir :: (Pully fvec a, Seqy m ivec a, Syntax a, Num a, MonadComp m)
    => fvec     -- ^ Filter coefficients
    -> ivec     -- ^ Input vector
    -> Seq m a  -- ^ Output vector
fir bs inp = recurrenceI (replicate (length bs) 0) inp $ \i ->
    scProd bs i

-- | IIR filter
iir :: ( Pully bvec a, Pully fvec a
       , Seqy m ivec a, Syntax a
       , Fractional a
       , MonadComp m
       )
    => a        -- ^ First feedback coefficient
    -> bvec     -- ^ Remaining feedback coefficients
    -> fvec     -- ^ Feedforward coefficients
    -> ivec     -- ^ Input vector
    -> Seq m a  -- ^ Output vector
iir a0 as bs inp = recurrenceIO
    (replicate (length bs) 0)
    inp
    (replicate (length as) 0)
    (\i o -> 1 / a0 * (scProd bs i - scProd as o))

-- | FIR filter for 'Pull' vectors
--
-- This version avoids creating a queue for previous inputs (since they are all
-- available anyway).
--
-- Note that each input element is referred many times, so the input should
-- normally be a 'Manifest'. In particular, this means that it is usually not a
-- good idea to compose 'firPull' without writing to memory in between.
firPull :: (Pully vec1 a, Pully vec2 a, Syntax a, Num a)
    => vec1    -- ^ Filter coefficients
    -> vec2    -- ^ Input vector
    -> Pull a  -- ^ Output vector
firPull bs = map (scProd bs . reverse) . tail . inits

