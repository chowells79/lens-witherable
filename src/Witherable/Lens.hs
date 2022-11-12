{-|
Module      : Witherable.Lens
Description : Tools for using the Witherable interface with lens
Copyright   : (c) Carl Howells, 2021
License     : MIT
Maintainer  : chowells79@gmail.com

-}
module Witherable.Lens where


import Data.Functor.Identity (Identity(runIdentity))

import Witherable (Witherable(wither))

import Witherable.Lens.Withering


-- | A variant on 'traverse' that allows the targets to be filtered
-- out of the 'Witherable' structure. Note that this introduces a
-- change in types down the lens composition chain, which means that
-- it is not a a valid optic at all.  The use of 'Withering' in the
-- changed type also means that standard lens combinators don't fit
--
-- To address these issues, you can use 'unwithered' to strip the
-- 'Withering' type back out. This allows the composed optic to be
-- used with standard combinators from lens. In addition, the sequence
-- @'withered' . 'unwithered'@ will act like a type-restricted version
-- of 'traverse' for all lawful instances of 'Witherable'.
--
-- In some sense, this is a 'catch'-like combinator. This marks the
-- point where removing elements stops propagating and actually
-- modifies the structure being focused.
withered
    :: (Applicative f, Witherable t)
    => (a -> Withering f b) -> t a -> f (t b)
withered f = wither (runWithering . f)

-- | Restore types in a lens composition chain that has had
-- 'Withering' introduced. Makes no changes to what elements are
-- focused on.
unwithered :: Functor f => (a -> f b) -> a -> Withering f b
unwithered f s = Withering (fmap Just (f s))

-- | A variant of withered for when you're already working in a
-- Withering chain and want to change what structure elements are
-- being removed from.
--
-- @'rewithered' = 'unwithered' . 'withered'@
rewithered
    :: (Applicative f, Witherable t)
    => (a -> Withering f b) -> t a -> Withering f (t b)
rewithered = unwithered . withered

-- | The trivial optic in a Withering chain that removes everything.
--
-- The arguments are unused.
decayed :: Applicative f => pafb -> s -> Withering f t
decayed _ _ = empty

-- | Remove elements from the current 'Withering' context if they
-- don't match the predicate.
guarded
    :: Applicative f
    => (a -> Bool) -> (a -> Withering f b)
    -> a -> Withering f b
guarded p f a
    | p a = f a
    | otherwise = empty


-- | Remove elements matched by a specific 'Withering' context if they
-- don't match a predicate.
filterOf
    :: ((a -> Withering Identity a) -> s -> Identity s)
    -> (a -> Bool) -> s -> s
filterOf w p = runIdentity . w (guarding p)
  where
    guarding p a
        | p a = pure a
        | otherwise = empty
infix 2 `filterOf`

-- | Transform and filter elements matched by a specific 'Withering'
-- context, a la 'Data.Maybe.mapMaybe'.
mapMaybeOf
    :: ((a -> Withering Identity b) -> s -> Identity t)
    -> (a -> Maybe b) -> s -> t
mapMaybeOf w p = runIdentity . w (Withering . pure . p)
infix 2 `mapMaybeOf`

-- | Transform and effectfully filter elements matched by a specific
-- 'Withering' context, a la 'wither'.
witherOf
    :: ((a -> Withering f b) -> s -> f t)
    -> (a -> f (Maybe b)) -> s -> f t
witherOf w p = w (Withering . p)
infix 2 `witherOf`
