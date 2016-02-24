-- | Validated values
--
-- 'Validated' is similar to 'Feldspar.Option.Option', with the difference that
-- 'Validated' does not guarantee that invalid values are not evaluated.
-- Therefore, 'Validated' should not be used to guard operations from illegal
-- use (e.g. array bounds checking).
--
-- Still, the operations try to defer evaluation of invalid values as much as
-- possible.

module Feldspar.Validated where



-- Since there's no guarantee that invalid values are not evaluated, there is
-- no point in hiding the implementation. Having access to the implementation
-- means that it's possible to take shortcuts. For example, `fmap` could not
-- have been implemented without opening up the representation.



import Prelude ()

import Language.Syntactic

import Feldspar hiding (desugar, sugar)
import Feldspar.Representation



-- | A value that can be valid or invalid
data Validated a = Validated (Data Bool) a

instance Functor Validated
  where
    fmap f (Validated valid a) = Validated valid (f a)

instance Applicative Validated
  where
    pure  = return
    (<*>) = ap

instance Monad Validated
  where
    return = Validated true
    Validated valid a >>= k = Validated (valid && valid') b
      where
        Validated valid' b = k a

instance Syntax a => Syntactic (Validated a)
  where
    type Domain (Validated a)   = FeldDomain
    type Internal (Validated a) = (Bool, Internal a)
    desugar (Validated valid a) = desugar (valid,a)
    sugar = uncurry Validated . sugar

-- | 'toValue' will force the value even if it's invalid
instance Forcible a => Forcible (Validated a)
  where
    type ValueRep (Validated a) = (Data Bool, ValueRep a)
    toValue (Validated valid a) = toValue (valid,a)
    fromValue = uncurry Validated . fromValue

-- | Create a validated value. Note that the value may get evaluated even if the
-- condition is false.
validWhen :: Data Bool -> a -> Validated a
validWhen = Validated

-- | Invalid value
invalid :: Syntax a => Validated a
invalid = Validated false example

-- | Deconstruct an 'Validated' value
validated :: Syntax b
    => b         -- ^ Invalid case
    -> (a -> b)  -- ^ Valid case
    -> Validated a
    -> b
validated no yes (Validated valid a) = valid ? yes a $ no

-- | Deconstruct an 'Validated' value
caseValidated :: Syntax b
    => Validated a
    -> b         -- ^ Invalid case
    -> (a -> b)  -- ^ Valid case
    -> b
caseValidated v no yes = validated no yes v

fromValidated :: Syntax a
    => Validated a
    -> a  -- ^ Value to return in case the first arg. is invalid
    -> a
fromValidated v def = caseValidated v def id

-- | Deconstruct an 'Validated' value
validatedM :: MonadComp m
    => m ()         -- ^ Invalid case
    -> (a -> m ())  -- ^ Valid case
    -> Validated a
    -> m ()
validatedM no yes (Validated valid a) = iff valid (yes a) no

-- | Deconstruct an 'Validated' value
caseValidatedM :: MonadComp m
    => Validated a
    -> m ()         -- ^ Invalid case
    -> (a -> m ())  -- ^ Valid case
    -> m ()
caseValidatedM v no yes = validatedM no yes v

