{-# LANGUAGE CPP #-}

-- Copyright (c) 2009-2011, ERICSSON AB
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

-- | Optional values

module Feldspar.Option where



import Prelude ()

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad

import qualified Language.Syntactic as Syntactic

import Feldspar.Representation
import Feldspar



-- | Optional value
--
-- (Analogous to 'Maybe' in normal Haskell.)
data Option a = Option { isSome :: Data Bool, fromSome :: a }

instance Syntax a => Syntactic.Syntactic (Option a)
  where
    type Domain (Option a)   = FeldDomain
    type Internal (Option a) = (Bool, Internal a)
    desugar = Syntactic.desugar . desugarOption . fmap Syntactic.resugar
    sugar   = fmap Syntactic.resugar . sugarOption . Syntactic.sugar

instance Functor Option
  where
    fmap f opt = opt {fromSome = f (fromSome opt)}

instance Applicative Option
  where
    pure  = return
    (<*>) = ap

instance Monad Option
  where
    return = some
    a >>= k = b { isSome = isSome a && isSome b }
      where
        b = k (fromSome a)
      -- TODO If `isSome` was a difference list, the condition would always be
      --      right-nested which would ensure maximal benefit from
      --      short-cirtuiting (I think).

-- | One-layer desugaring of 'Option'
desugarOption :: Type a => Option (Data a) -> Data (Bool,a)
desugarOption a = resugar (isSome a, fromSome a)

-- | One-layer sugaring of 'Option'
sugarOption :: Type a => Data (Bool,a) -> Option (Data a)
sugarOption (resugar -> (valid,a)) = Option valid a

-- | Analogous to 'Just' in normal Haskell
some :: a -> Option a
some = Option true

-- | Analogous to 'Nothing' in normal Haskell
none :: Syntax a => Option a
none = Option false example

-- | Analogous to 'maybe' in normal Haskell
option :: Syntax b => b -> (a -> b) -> Option a -> b
option noneCase someCase opt = isSome opt
    ? someCase (fromSome opt)
    $ noneCase

-- | Monadic version of 'option'
optionM :: (MonadComp m, Syntax b) => m b -> (a -> m b) -> Option a -> m b
optionM noneCase someCase opt = ifE (isSome opt)
    (someCase (fromSome opt))
    (noneCase)

-- | Assert that an optional value is valid, and return its value
fromSomeAssert :: MonadComp m => String -> Option a -> m a
fromSomeAssert msg o = do
    assert (isSome o) msg
    return $ fromSome o

-- | Take the most defined of two optional values, with preference to the first
-- one
oplus :: Syntax a => Option a -> Option a -> Option a
oplus a b = isSome a ? a $ b

