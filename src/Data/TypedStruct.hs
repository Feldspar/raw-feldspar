{-# LANGUAGE UndecidableInstances #-}

-- | Typed binary tree structures

module Data.TypedStruct where



import Control.Monad.Identity



--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

-- | Typed binary tree structure
--
-- The predicate @pred@ is assumed to rule out pairs. Functions like
-- 'extractSingle' and 'zipStruct' rely on this assumption.
data Struct pred con a
  where
    Single :: pred a => con a -> Struct pred con a
    Two    :: Struct pred con a -> Struct pred con b -> Struct pred con (a,b)

-- It would have been nice to add a constraint `IsPair a ~ False` to `Single`,
-- so that one wouldn't have to rely on @pred@ to rule out pairs. However,
-- attempting to do so lead to very strange problems in the rest of the Feldspar
-- implementation, so in the end I abandoned this extra safety.
--
-- The problems were strange enough that it seems likely they may be due to a
-- bug in GHC (7.10.2). So it might be worthwhile to try this again in a later
-- version.
--
-- Note however, that `IsPair a ~ False` on `Single` is not enough to please the
-- completeness checker for functions like `extractSingle` in GHC 7.10. Maybe
-- the new completeness checker in GHC 8 will be satisfied?

-- | Create a 'Struct' from a 'Struct' of any container @c@ and a structured
-- value @a@
--
-- For example:
--
-- @
-- `toStruct` (`Two` (`Single` `Proxy`) (`Single` `Proxy`)) (False,'a')
--   ==
-- Two (Single (Identity False)) (Single (Identity 'a'))
-- @
toStruct :: Struct p c a -> a -> Struct p Identity a
toStruct rep = go rep . Identity
  where
    go :: Struct p c a -> Identity a -> Struct p Identity a
    go (Single _) i = Single i
    go (Two ra rb) (Identity (a,b)) =
        Two (go ra (Identity a)) (go rb (Identity b))



--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------

-- | Extract the value of a 'Single'
extractSingle :: pred a => Struct pred c a -> c a
extractSingle (Single a) = a

-- | Map over a 'Struct'
mapStruct :: forall pred c1 c2 b
    .  (forall a . pred a => c1 a -> c2 a)
    -> Struct pred c1 b
    -> Struct pred c2 b
mapStruct f = go
  where
    go :: Struct pred c1 a -> Struct pred c2 a
    go (Single a) = Single (f a)
    go (Two a b)  = Two (go a) (go b)

-- | Monadic map over a 'Struct'
mapStructA :: forall m pred c1 c2 b . Applicative m
    => (forall a . pred a => c1 a -> m (c2 a))
    -> Struct pred c1 b -> m (Struct pred c2 b)
mapStructA f = go
  where
    go :: Struct pred c1 a -> m (Struct pred c2 a)
    go (Single a) = Single <$> (f a)
    go (Two a b)  = Two <$> go a <*> go b

-- | Map over a 'Struct'
mapStructA_ :: forall m pred cont b . Applicative m =>
    (forall a . pred a => cont a -> m ()) -> Struct pred cont b -> m ()
mapStructA_ f = go
  where
    go :: Struct pred cont a -> m ()
    go (Single a) = f a
    go (Two a b)  = go a *> go b

-- mapStructM_ :: forall m pred cont b . Monad m =>
--     (forall a . pred a => cont a -> m ()) -> Struct pred cont b -> m ()
-- mapStructM_ f = sequence_ . listStruct f
  -- This doesn't work for some reason, only if `pred` is constrained to a
  -- concrete type. (On the other hand, using `listStruct` is probably less
  -- efficient due to the use of `++`.)

-- | Fold a 'Struct' to a list
listStruct :: forall pred cont b c .
    (forall y . pred y => cont y -> c) -> Struct pred cont b -> [c]
listStruct f = go
  where
    go :: Struct pred cont a -> [c]
    go (Single a) = [f a]
    go (Two a b)  = go a ++ go b

-- | Zip two 'Struct's
zipStruct :: forall pred c1 c2 c3 b
    . (forall a . pred a => c1 a -> c2 a -> c3 a)
    -> Struct pred c1 b
    -> Struct pred c2 b
    -> Struct pred c3 b
zipStruct f = go
  where
    go :: Struct pred c1 a -> Struct pred c2 a -> Struct pred c3 a
    go (Single a) (Single b) = Single (f a b)
    go (Two a b) (Two c d)   = Two (go a c) (go b d)

-- | Zip two 'Struct's to a list
zipListStruct :: forall pred c1 c2 b r
    . (forall a . pred a => c1 a -> c2 a -> r)
    -> Struct pred c1 b
    -> Struct pred c2 b
    -> [r]
zipListStruct f = go
  where
    go :: Struct pred c1 a -> Struct pred c2 a -> [r]
    go (Single a) (Single b) = [f a b]
    go (Two a b) (Two c d)   = go a c ++ go b d

-- | Compare two 'Struct's using a function that compares the 'Single' elements.
-- If the structures don't match, 'False' is returned.
compareStruct :: forall pred c1 c2 c d
    . (forall a b . (pred a, pred b) => c1 a -> c2 b -> Bool)
    -> Struct pred c1 c
    -> Struct pred c2 d
    -> Bool
compareStruct f = go
  where
    go :: Struct pred c1 a -> Struct pred c2 b -> Bool
    go (Single a) (Single b) = f a b
    go (Two a b) (Two c d)   = go a c && go b d

-- | Lift a function operating on containers @con@ to a function operating on
-- 'Struct's.
liftStruct :: (pred a, pred b) =>
    (con a -> con b) -> Struct pred con a -> Struct pred con b
liftStruct f (Single a) = Single (f a)

-- | Lift a function operating on containers @con@ to a function operating on
-- 'Struct's.
liftStruct2 :: (pred a, pred b, pred c)
    => (con a -> con b -> con c)
    -> Struct pred con a -> Struct pred con b -> Struct pred con c
liftStruct2 f (Single a) (Single b) = Single (f a b)

