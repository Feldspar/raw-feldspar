{-# LANGUAGE UndecidableInstances #-}

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

import Data.TypedStruct

import Language.Syntactic
import Language.Syntactic.Functional.Tuple

import Feldspar hiding (desugar, sugar)
import Feldspar.Representation
import Feldspar.Primitive.Representation (Primitive)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | A value that can be valid or invalid
data Validated exp a = Validated (exp Bool) a

instance Functor (Validated exp)
  where
    fmap f (Validated valid a) = Validated valid (f a)

instance BOOL exp => Applicative (Validated exp)
  where
    pure  = return
    (<*>) = ap

instance BOOL exp => Monad (Validated exp)
  where
    return = Validated true
    Validated valid a >>= k = Validated (valid && valid') b
      where
        Validated valid' b = k a

instance ( Syntax exp a
         , Syntax exp (exp Bool)
         , Internal (exp Bool) ~ Bool
         , DomainOf exp ~ (sup :&: TypeRepFun)
         , Tuple :<: sup
         )
    => Syntactic (Validated exp a)
  where
    type Domain   (Validated exp a) = DomainOf exp
    type Internal (Validated exp a) = (Bool, Internal a)
    
    desugar (Validated valid a) = desugar (valid, a)
    sugar = uncurry Validated . sugar
{-
-- | 'toValue' will force the value even if it's invalid
instance ( Forcible a
         , Forcible (exp Bool)
         , ValueRep (exp Bool) ~ exp Bool
         , ExprOf a ~ ExprOf (exp Bool))
    => Forcible (Validated exp a)
  where
    type ValueRep (Validated exp a) = (exp Bool, ValueRep a)
    toValue (Validated valid a) = toValue (valid, a)
    fromValue = uncurry Validated . fromValue
-}
-- | Create a validated value. Note that the value may get evaluated even if the
-- condition is false.
validWhen :: exp Bool -> a -> Validated exp a
validWhen = Validated

-- | Invalid value
invalid
  :: ( BOOL exp
     , Domain a ~ (sup :&: TypeRepFun)
     , Primitive :<: sup)
  => Syntax exp a => Validated exp a
invalid = Validated false example

-- | Deconstruct an 'Validated' value
validated :: (COND exp, Syntax exp b)
    => b         -- ^ Invalid case
    -> (a -> b)  -- ^ Valid case
    -> Validated exp a
    -> b
validated no yes (Validated valid a) = valid ? yes a $ no

-- | Deconstruct an 'Validated' value
caseValidated :: (COND exp, Syntax exp b)
    => Validated exp a
    -> b         -- ^ Invalid case
    -> (a -> b)  -- ^ Valid case
    -> b
caseValidated v no yes = validated no yes v

fromValidated :: (COND exp, Syntax exp a)
    => Validated exp a
    -> a  -- ^ Value to return in case the first arg. is invalid
    -> a
fromValidated v def = caseValidated v def id

-- | Deconstruct an 'Validated' value
validatedM :: MonadComp exp m
    => m ()         -- ^ Invalid case
    -> (a -> m ())  -- ^ Valid case
    -> Validated exp a
    -> m ()
validatedM no yes (Validated valid a) = iff valid (yes a) no

-- | Deconstruct an 'Validated' value
caseValidatedM :: MonadComp exp m
    => Validated exp a
    -> m ()         -- ^ Invalid case
    -> (a -> m ())  -- ^ Valid case
    -> m ()
caseValidatedM v no yes = validatedM no yes v
