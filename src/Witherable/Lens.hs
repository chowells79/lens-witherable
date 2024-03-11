{-|
Module      : Witherable.Lens
Description : Tools for using the Witherable interface with lens
Copyright   : (c) Carl Howells, 2021-2022
License     : MIT
Maintainer  : chowells79@gmail.com

-}
module Witherable.Lens where


import Data.Functor.Identity (Identity(runIdentity))

import Witherable (Witherable(wither))

import Witherable.Lens.Withering

import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')

import Data.Set (Set)
import qualified Data.Set as S

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as H


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
-- In some sense, this is a @catch@-like combinator. This marks the
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
-- don't match the predicate. This is similar in concept to @filtered@
-- from lens but instead of merely removing non-matching
-- targets from the traversal, it removes those targets and their
-- parents up to the next 'withered' combinator.
guarded
    :: Applicative f
    => (a -> Bool) -> (a -> Withering f b)
    -> a -> Withering f b
guarded p f a
    | p a = f a
    | otherwise = empty



--------------------------------------------------------
-- Port deprecated definitions over from Data.Witherable

-- | Transform and effectfully filter elements matched by a specific
-- 'Withering' context, a la 'wither'.
--
-- >>> witherOf (withered . _1) (\x -> do b <- readLn ; if b then pure (Just (show x)) else pure Nothing) [(1,2),(2,3),(3,4)]
-- False
-- True
-- True
-- [("2",3),("3",4)]
witherOf
    :: ((a -> Withering f b) -> s -> f t)
    -> (a -> f (Maybe b)) -> s -> f t
witherOf w p = w (Withering . p)

-- | A version of 'witherOf' with arguments re-arranged.
forMaybeOf
    :: ((a -> Withering f b) -> s -> f t)
    -> s -> (a -> f (Maybe b)) -> f t
forMaybeOf w s p = witherOf w p s

-- | Transform and filter elements matched by a specific 'Withering'
-- context, a la 'Data.Maybe.mapMaybe'. See 'witherOf' for a more
-- flexible version that works within arbitrary 'Applicative' effects.
--
-- >>> mapMaybeOf (withered . _1) (\x -> if even x then Just (show x) else Nothing) [(1,2),(2,4),(3,6),(4,8)]
-- [("2",4),("4",8)]
mapMaybeOf
    :: ((a -> Withering Identity b) -> s -> Identity t)
    -> (a -> Maybe b) -> s -> t
mapMaybeOf w p = runIdentity . w (Withering . pure . p)

-- | Filter @Nothing@ values out of a structure, like
-- 'Data.Maybe.catMaybes'.
--
-- >>> catMaybesOf withered [Just 1, Nothing, Just 2]
-- [1,2]
--
-- >>> catMaybesOf (withered . _2) [("a", Just 1), ("b", Nothing), ("c", Just 2)]
-- [("a",1),("c",2)]
catMaybesOf
    :: ((Maybe a -> Withering Identity a) -> s -> Identity t)
    -> s -> t
catMaybesOf w = runIdentity . w toWithering
  where
    toWithering Nothing = empty
    toWithering (Just x) = pure x

-- | Remove elements matched by a specific 'Withering' context if they
-- don't match a predicate returning a result in an arbitrary
-- Applicative context.
--
-- >>> filterAOf (withered . _1) (const readLn) [(1,2),(2,4),(3,6),(4,8)]
-- False
-- True
-- True
-- False
-- [(2,4),(3,6)]
filterAOf
    :: Applicative f
    => ((a -> Withering f a) -> s -> f s)
    -> (a -> f Bool) -> s -> f s
filterAOf w p = w toWitheringA
  where
    toWitheringA a = Withering $ (\x -> if x then Just a else Nothing) <$> p a

-- | Remove elements matched by a specific 'Withering' context if they
-- don't match a predicate.
--
-- >>> filterOf (withered . _1) even [(1,2),(2,4),(3,6),(4,8)]
-- [(2,4),(4,8)]
filterOf
    :: ((a -> Withering Identity a) -> s -> Identity s)
    -> (a -> Bool) -> s -> s
filterOf w p = runIdentity . w toWithering
  where
    toWithering a
        | p a = pure a
        | otherwise = empty

-- | Removes duplicates from a structure based on the focused element.
--
-- >>> ordNubOf (traverse . withered . _2) [[("z",3),("q",3)],[("apple", 1), ("bat", 1), ("cat", 2), ("dog", 2)]]
-- [[("z",3)],[("apple",1),("cat",2)]]
ordNubOf
    :: Ord a
    => ((a -> Withering (State (Set a)) a) -> s -> State (Set a) s)
    -> s -> s
ordNubOf w = ordNubOnOf w id

-- | Removes duplicates from a structure based on applying a function
-- to the focused element.
--
-- >>> ordNubOnOf (withered . _2) (`div` 2) [("apple", 1), ("bat", 2), ("cat", 3), ("dog", 4)]
-- [("apple",1),("bat",2),("dog",4)]
ordNubOnOf
    :: Ord b
    => ((a -> Withering (State (Set b)) a) -> s -> State (Set b) s)
    -> (a -> b)
    -> s -> s
ordNubOnOf w f s = evalState (w doWithering s) S.empty
  where
    doWithering x = Withering $ do
        let fx = f x
        seen <- gets (S.member fx)
        if seen
            then pure Nothing
            else do
                modify' (S.insert fx)
                pure (Just x)

-- | Removes duplicates from a structure based on the focused
-- element. Often will be more efficient than 'ordNubOf' if your data
-- type supports 'Hashable'
--
-- >>> hashNubOf (traverse . withered . _2) [[("z",3),("q",3)],[("apple", 1), ("bat", 1), ("cat", 2), ("dog", 2)]]
-- [[("z",3)],[("apple",1),("cat",2)]]
hashNubOf
    :: (Eq a, Hashable a)
    => ((a -> Withering (State (HashSet a)) a) -> s -> State (HashSet a) s)
    -> s -> s
hashNubOf w = hashNubOnOf w id

-- | Removes duplicates from a structure based on applying a function
-- to the focused element. Often will be more efficient than
-- 'ordNubOnOf' if your data type supports 'Hashable'
--
-- >>> hashNubOnOf (withered . _2) (`div` 2) [("apple", 1), ("bat", 2), ("cat", 3), ("dog", 4)]
-- [("apple",1),("bat",2),("dog",4)]
hashNubOnOf
    :: (Eq b, Hashable b)
    => ((a -> Withering (State (HashSet b)) a) -> s -> State (HashSet b) s)
    -> (a -> b)
    -> s -> s
hashNubOnOf w f s = evalState (w doWithering s) H.empty
  where
    doWithering x = Withering $ do
        let fx = f x
        seen <- gets (H.member fx)
        if seen
            then pure Nothing
            else do
                modify' (H.insert fx)
                pure (Just x)
