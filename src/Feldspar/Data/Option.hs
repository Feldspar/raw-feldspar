{-# LANGUAGE PolyKinds #-}

-- | Optional values

module Feldspar.Data.Option
  ( OptionT
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
  ) where



import Prelude ()

import Control.Monad.Operational.Higher
import Control.Monad.Identity
import Control.Monad.Trans

import Language.Syntactic

import Feldspar
import Feldspar.Representation



data Opt fs a
  where
    None  :: String -> Opt (Param1 prog) a
    Guard :: String -> Data Bool -> Opt (Param1 prog) ()

instance HFunctor Opt
  where
    hfmap f (None msg)    = None msg
    hfmap f (Guard msg c) = Guard msg c

-- | Transformer version of 'Option'
newtype OptionT m a = Option { unOption :: ProgramT Opt Param0 m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Optional value, analogous to @`Either` `String` a@ in normal Haskell
type Option = OptionT Identity

instance MonadComp m => MonadComp (OptionT m)
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

-- | Construct a missing 'Option' value (analogous to 'Left' in normal Haskell)
none :: String -> OptionT m a
none = Option . singleton . None

-- | Construct a present 'Option' value (analogous to 'Right' in normal Haskell)
some :: Monad m => a -> OptionT m a
some = return

-- | Construct an 'Option' that is either 'none' or @`some` ()@ depending on
-- the Boolean guard
--
-- In the expression @`guardO` c `>>` rest@, the action @rest@ will not be
-- executed unless @c@ is true.
guardO :: String -> Data Bool -> OptionT m ()
guardO msg c = Option $ singleton $ Guard msg c

-- | Construct an 'Option' from a guard and a value. The value will not be
-- evaluated if the guard is false.
guarded :: Monad m => String -> Data Bool -> a -> OptionT m a
guarded msg c a = guardO msg c >> return a

rebuildOption :: Monad m => Option a -> OptionT m a
rebuildOption = interpretWithMonad go . unOption
  where
    go :: Opt (Param1 (OptionT m)) a -> OptionT m a
    go (None msg)    = none msg
    go (Guard msg c) = guardO msg c

-- | Deconstruct an 'Option' value (analogous to 'either' in normal Haskell)
option :: Syntax b
    => (String -> b)  -- ^ 'none' case
    -> (a -> b)       -- ^ 'some' case
    -> Option a
    -> b
option noneCase someCase (Option opt) = go (view opt)
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = cond c (go $ view $ k ()) (noneCase msg)

-- | Deconstruct an 'Option' value
caseOption :: Syntax b
    => Option a
    -> (String -> b)  -- ^ 'none' case
    -> (a -> b)       -- ^ 'some' case
    -> b
caseOption o n s = option n s o

-- | Extract the value of an 'Option' that is assumed to be present
fromSome :: Syntax a => Option a -> a
fromSome = option (const example) id

-- | Deconstruct an 'Option' value (analogous to 'maybe' in normal Haskell)
optionM :: MonadComp m
    => (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> Option a
    -> m ()
optionM noneCase someCase (Option opt) = go $ view opt
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = iff c (go $ view $ k ()) (noneCase msg)

-- | Deconstruct an 'Option' value
caseOptionM :: MonadComp m
    => Option a
    -> (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> m ()
caseOptionM o n s = optionM n s o

-- | Extract the value of an 'Option' that is assumed to be present
fromSomeM :: (Syntax a, MonadComp m) => Option a -> m a
fromSomeM opt = do
    r <- newRef
    caseOptionM opt
        (const $ return ())
        (setRef r)
    unsafeFreezeRef r

-- | Deconstruct an 'OptionT' value
optionT :: MonadComp m
    => (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> OptionT m a
    -> m ()
optionT noneCase someCase (Option opt) = go =<< viewT opt
  where
    go (Return a)           = someCase a
    go (None msg    :>>= k) = noneCase msg
    go (Guard msg c :>>= k) = iff c (go =<< viewT (k ())) (noneCase msg)

-- | Deconstruct an 'OptionT' value
caseOptionT :: MonadComp m
    => OptionT m a
    -> (String -> m ())  -- ^ 'none' case
    -> (a -> m ())       -- ^ 'some' case
    -> m ()
caseOptionT o n s = optionT n s o

-- | Extract the value of an 'OptionT' that is assumed to be present
fromSomeT :: (Syntax a, MonadComp m) => OptionT m a -> m a
fromSomeT opt = do
    r <- newRef
    caseOptionT opt
        (const $ return ())
        (setRef r)
    unsafeFreezeRef r

