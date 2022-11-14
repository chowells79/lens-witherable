Integrate witherable and lens.

Based on the ideas from https://chrispenner.ca/posts/witherable-optics
but I think this version has overall better ergonomics. This library
uses an approach based on an additional combinator to restore full
compatibility with lens combinators like `set` and `over`, which the
approach in the blog has trouble with. Additionally, this approach
gets rid of `Maybe` wrappers in results that are always `Just`.

This library is based on a variant of `Traversal`s with the type
`forall f. Applicative f => ((a-> Withering f b) -> s -> f t)`. Note
that this type is absolutely not an optic due to the `Withering`
wrapper, but it's really close. It composes on either side with lenses
and traversals with `(.)` and that composition maintains the filtering
type. No type alias is provided for this type because type aliases
containing a `forall` cause problems with subsumption.

The core tool for creating values with these types is

```haskell
withered
    :: (Applicative f, Witherable t)
    => (a -> Withering f b) -> t a -> f (t b)
```

One tool for working with the types is

```haskell
mapMaybeOf
    :: ((a -> Withering Identity b) -> s -> Identity t)
    -> (a -> Maybe b) -> s -> t
```

These can be used like such

```haskell
ghci> let z = M.fromList [('a', ["1", "2", "3"]), ('b', ["4", "Alpaca", "6"])]

ghci> z & mapMaybeOf (traverse . withered) (readMaybe :: String -> Maybe Int)
fromList [('a',[1,2,3]),('b',[4,6])]

ghci> z & mapMaybeOf (withered . traverse) (readMaybe :: String -> Maybe Int)
fromList [('a',[1,2,3])]
```

As those results demonstrate, the location of the `withered`
combinator controls where the pruning stops. It functions as a kind of
catch combinator in this sense.

Of course, `filterOf` is provided as well

```haskell
filterOf
    :: ((a -> Withering Identity a) -> s -> Identity s)
    -> (a -> Bool) -> s -> s
```

```haskell
ghci> [[1, 2, 3], [4, 5, 6]] & filterOf (traverse . withered) even
[[2],[4,6]]
```

Traversals that might fail can be combined with the `decayed`
combinator to cause them to completely remove a value that they
otherwise would ignore

```haskell
ghci> [('a', Right 1), ('b', Left 2), ('c', Left 3)] & filterOf (withered . _2 . _Left) even
[('a',Right 1),('b',Left 2)]

ghci> [('a', Right 1), ('b', Left 2), ('c', Left 3)] & filterOf (withered . _2 . (_Left `failing` decayed)) even
[('b',Left 2)]
```

In the first case, the `'a'` value is returned because the filter
doesn't find any non-even values under the `Right` value when it's
looking under the `_Left` prism. In the second case, the failure of
the `_Left` prism results in falling back on `decayed` to remove the
branch.

Compatibility with normal lens operations can be restored with
`unwithered`. This can be especially useful when combined with
`guarded`, which acts somewhat like lens's `filtered` but it prunes
the structure up to the next `withered`.

```haskell
unwithered :: Functor f => (a -> f b) -> a -> Withering f b

guarded
    :: Applicative f
    => (a -> Bool) -> (a -> Withering f b)
    -> a -> Withering f b
```

```haskell
ghci> [[1,2],[],[1,3,9],[],[],[6,4,7],[]] & withered . guarded (not . null) . unwithered . traverse +~ 10
[[11,12],[11,13,19],[16,14,17]]
```

Note that pruning out the empty lists happens in-line with the
modification of the other elements. An additional combinator is
provided combining `withered` and `unwithered` for when you want to
change the pruning depth

```haskell
rewithered
    :: (Applicative f, Witherable t)
    => (a -> Withering f b) -> t a -> Withering f (t b)
```

```haskell
ghci> [[1,2],[],[1,3,9],[],[],[6,4,7],[]] & (withered . guarded (not . null) . rewithered . guarded even . unwithered) +~ 10
[[12],[],[16,14]]
```

In that example, the first `guarded` strips out null lists in the
input. The `rewithered` descends into the sublists and sets each of
them as the catch point for later pruning. The second `guarded` strips
out all non-even entries in each list. The `unwithered` restores
compatibility with lens combinators like `(+~)`, which is used to
modify every remaining focused value.

