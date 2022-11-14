{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

{-|
Module      : Witherable.Lens.Withering
Description : MaybeT replacement type
Copyright   : (c) Carl Howells, 2021
License     : MIT
Maintainer  : chowells79@gmail.com

This module contains a replacement for @MaybeT@ intended for use in
lens-like contexts. The important difference from @MaybeT@ is that
'Withering' drops the short-circuiting behavior that requires 'Monad'
constraints.
-}
module Witherable.Lens.Withering (Withering(..), empty) where

import Control.Applicative (liftA2)

-- | A replacement for @MaybeT@ with no short-circuiting
-- behavior. This allows its 'Applicative' instance to not require @f@
-- to be an instance of 'Monad'.
newtype Withering f a = Withering { runWithering :: f (Maybe a) }

deriving instance Eq (f (Maybe a)) => Eq (Withering f a)
deriving instance Ord (f (Maybe a)) => Ord (Withering f a)
deriving instance Show (f (Maybe a)) => Show (Withering f a)

instance Functor f => Functor (Withering f) where
    fmap f (Withering x) = Withering (fmap (fmap f) x)

instance Applicative f => Applicative (Withering f) where
    pure x = Withering (pure (Just x))
    Withering f <*> Withering x = Withering (liftA2 (<*>) f x)

-- | A 'Withering' value wrapping 'Nothing'. This cannot be part of an
-- 'Control.Applicative.Alternative' instance for 'Withering' because
-- it needs to be available with only an 'Applicative' constraint on
-- @f@, and any lawful 'Control.Applicative.Alternative' instance
-- would require more structure than that.
empty :: Applicative f => Withering f a
empty = Withering (pure Nothing)
