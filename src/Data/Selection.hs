-- | Data structure for describing selections of values

module Data.Selection
  ( Selection
  , includes
  , selectBy
  , empty
  , universal
  , select
  , union
  , intersection
  , difference
  , allExcept
  ) where



-- | Selection: description of a set of values
data Selection a = Selection (a -> Bool)

instance Semigroup (Selection a)
  where
    (<>) = mappend

-- |
-- @
-- `mempty`  = `empty`
-- `mappend` = `union`
-- @
instance Monoid (Selection a)
  where
    mempty  = empty
    mappend = union

-- | Check whether a value is included in a selection
includes :: Selection a -> a -> Bool
includes (Selection p) = p

-- | Select the values that fulfill a predicate
selectBy :: (a -> Bool) -> Selection a
selectBy = Selection

-- | Empty selection
empty :: Selection a
empty = Selection $ \_ -> False

-- | Select all values
universal :: Selection a
universal = Selection $ \_ -> True

-- | Union of selections
union :: Selection a -> Selection a -> Selection a
union s t = Selection $ \a -> includes s a || includes t a

-- | Intersection of selections
intersection :: Selection a -> Selection a -> Selection a
intersection s t = Selection $ \a -> includes s a && includes t a

-- | Difference of selections
difference :: Selection a -> Selection a -> Selection a
difference s t = Selection $ \a -> includes s a && not (includes t a)

-- | Create a classification from a list of elements
select :: Eq a => [a] -> Selection a
select as = selectBy (`elem` as)

-- | Select all values except those in the given list
allExcept :: Eq a => [a] -> Selection a
allExcept = difference universal . select

