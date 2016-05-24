{-# LANGUAGE PolyKinds #-}

-- | Optional values

module Feldspar.Option
  {-( OptionT
  , Option
  , none
  , some
  , guardO
  , guarded
  , rebuildOption
  , option
  , caseOption
  , fromSome
  , optionM
  , caseOptionM
  , fromSomeM
  , optionT
  , caseOptionT
  , fromSomeT
  )-} where



import Prelude ()

import Control.Monad.Operational.Higher
import Control.Monad.Identity
import Control.Monad.Trans

import Language.Syntactic
import Language.Embedded.Imperative (FreeExp)

import Feldspar
import Feldspar.Representation

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Opt fs a
  where
    None  :: String -> Opt (Param2 prog exp) a
    Guard :: String -> exp Bool -> Opt (Param2 prog exp) ()

instance HFunctor Opt
  where
    hfmap f (None msg)    = None msg
    hfmap f (Guard msg c) = Guard msg c

-- | Transformer version of 'Option'
newtype OptionT exp m a = Option { unOption :: ProgramT Opt (Param1 exp) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Optional value, analogous to @`Either` `String` a@ in normal Haskell
type Option exp = OptionT exp Identity

{-
instance MonadComp exp m => MonadComp (OptionT exp m)
  where
    liftComp = lift . liftComp
    iff c t f = do
        okr <- initRef true
        lift $ iff c
            (optionT (\_ -> setRef okr false) return t)
            (optionT (\_ -> setRef okr false) return f)
        ok <- unsafeFreezeRef okr
        guardO "iff: none" ok
    for rng body = do
        okr <- initRef true
        lift $ for rng $ \i ->
            optionT (\_ -> setRef okr false >> break) return (body i)
        ok <- unsafeFreezeRef okr
        guardO "for: none" ok
    while cont body = do
        okr <- initRef true
        lift $ while
            (cont' okr)
            (optionT (\_ -> setRef okr false >> break) return body)
        ok <- unsafeFreezeRef okr
        guardO "while: none" ok
      where
        cont' okr = do
            cr <- newRef
            caseOptionT (cont >>= setRef cr) (\_ -> setRef okr false >> setRef cr false) return
            unsafeFreezeRef cr
-}
{-
instance Syntax a => Syntactic (Option a)
  where
    type Domain   (Option a) = FeldDomain
    type Internal (Option a) = (Bool, Internal a)

    desugar = unData . option
        (\_ -> Feldspar.desugar (false,example :: (Data (Internal a))))
        (\a -> Feldspar.desugar (true,a))

    sugar o = guarded "sugar: none" valid a
      where
        (valid,a) = Feldspar.sugar $ Data o

instance Syntax a => Forcible (Option a)
  where
    type ValueRep (Option a) = (Data Bool, Data (Internal a))
    toValue o = do
        valid <- initRef false
        r     <- initRef (example :: a)
        caseOptionM o
          (\_ -> return ())
          (\b -> setRef valid true >> setRef r b)
        (,) <$> unsafeFreezeRef valid <*> unsafeFreezeRef r
    fromValue (valid,a) = guarded "fromIStore: none" valid (Feldspar.sugar a)
  -- Ideally, one should use `Storable` instead of the `Syntax` constraint, and
  -- make `r` a `Store` instead of a reference. But the problem is that one
  -- would have to make use of `newStore` which needs a size argument. This is
  -- problematic because the size of the value is not known until inside
  -- `caseOptionM`.

instance (Storable a, Syntax a) => Storable (Option a)
  where
    type StoreRep (Option a)  = (Ref Bool, StoreRep a)
    type StoreSize (Option a) = StoreSize a
    newStoreRep _ s = do
        valid <- initRef false
        r     <- newStoreRep (Nothing :: Maybe a) s
        return (valid,r)
    initStoreRep o = do
        valid <- initRef false
        r     <- initStoreRep (example :: a)  -- TODO
        caseOptionM o
          (\_ -> return ())
          (\b -> writeStoreRep (valid,r) (true,b))
        return (valid,r)
    readStoreRep oRep = do
        (valid,a) <- readStoreRep oRep
        return $ guarded "readStoreRep: none" valid a
    unsafeFreezeStoreRep oRep = do
        (valid,a) <- unsafeFreezeStoreRep oRep
        return $ guarded "unsafeFreezeStoreRep: none" valid a
    writeStoreRep oRep@(valid,r) o = caseOptionM o
        (\_ -> setRef valid false)
        (\a -> writeStoreRep oRep (true,a))
    copyStoreRep _ = copyStoreRep (Nothing :: Maybe (Data Bool, a))
      -- Uses the instance `Storable (Data Bool, a)` for copying
-}

-- | Construct a missing 'Option' value (analogous to 'Left' in normal Haskell)
none :: String -> OptionT exp m a
none = Option . singleton . None

-- | Construct a present 'Option' value (analogous to 'Right' in normal Haskell)
some :: Monad m => a -> OptionT exp m a
some = return

-- | Construct an 'Option' that is either 'none' or @`some` ()@ depending on
-- the Boolean guard
--
-- In the expression @`guardO` c `>>` rest@, the action @rest@ will not be
-- executed unless @c@ is true.
guardO :: String -> exp Bool -> OptionT exp m ()
guardO msg c = Option $ singleton $ Guard msg c

-- | Construct an 'Option' from a guard and a value. The value will not be
-- evaluated if the guard is false.
guarded :: Monad m => String -> exp Bool -> a -> OptionT exp m a
guarded msg c a = guardO msg c >> return a
{-
rebuildOption :: Monad m => Option exp a -> OptionT exp m a
rebuildOption = interpretWithMonad go . unOption
  where
    go :: Opt (Param2 (OptionT m) exp) a -> OptionT exp m a
    go (None msg)    = none msg
    go (Guard msg c) = guardO msg c
-}

-- | Deconstruct an 'Option' value (analogous to 'either' in normal Haskell)
option :: (COND exp, Syntax exp b)
    => (String -> b)  -- ^ 'none' case
    -> (a -> b)       -- ^ 'some' case
    -> Option exp a
    -> b
option noneCase someCase (Option opt) = go (view opt)
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = cond c (go $ view $ k ()) (noneCase msg)

-- | Deconstruct an 'Option' value
caseOption :: (COND exp, Syntax exp b)
    => Option exp a
    -> (String -> b)  -- ^ 'none' case
    -> (a -> b)       -- ^ 'some' case
    -> b
caseOption o n s = option n s o

-- | Extract the value of an 'Option' that is assumed to be present
fromSome :: (COND exp, VAL exp, Syntax exp a) => Option exp a -> a
fromSome = option (const example) id

-- | Deconstruct an 'Option' value (analogous to 'maybe' in normal Haskell)
optionM :: MonadComp exp m
    => (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> Option exp a
    -> m ()
optionM noneCase someCase (Option opt) = go $ view opt
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = iff c (go $ view $ k ()) (noneCase msg)

-- | Deconstruct an 'Option' value
caseOptionM :: MonadComp exp m
    => Option exp a
    -> (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> m ()
caseOptionM o n s = optionM n s o

-- | Extract the value of an 'Option' that is assumed to be present
fromSomeM :: (Syntax exp a, MonadComp exp m, FreeExp exp, FreeDict exp) => Option exp a -> m a
fromSomeM opt = do
    r <- newRef
    caseOptionM opt
        (const $ return ())
        (setRef r)
    unsafeFreezeRef r

-- | Deconstruct an 'OptionT' value
optionT :: MonadComp exp m
    => (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> OptionT exp m a
    -> m ()
optionT noneCase someCase (Option opt) = go =<< viewT opt
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = iff c (go =<< viewT (k ())) (noneCase msg)

-- | Deconstruct an 'OptionT' value
caseOptionT :: MonadComp exp m
    => OptionT exp m a
    -> (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> m ()
caseOptionT o n s = optionT n s o

-- | Extract the value of an 'OptionT' that is assumed to be present
fromSomeT :: (Syntax exp a, MonadComp exp m, FreeExp exp, FreeDict exp) => OptionT exp m a -> m a
fromSomeT opt = do
    r <- newRef
    caseOptionT opt
        (const $ return ())
        (setRef r)
    unsafeFreezeRef r
