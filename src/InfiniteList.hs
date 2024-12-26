module InfiniteList where

import Prelude hiding (cycle)

{-
Exercise A

Given three lists xs, ys and zs in strictly increasing order, we have
	merge (merge xs ys) zs} = merge xs (merge ys zs)

Thus merge is associative. Assuming in addition that the first elements of xs,
ys and zs are in strictly increasing order, we also have
	xmerge (xmerge xs ys) zs = xmerge xs (xmerge ys zs)

Does it follow that in the expression foldr1 xmerge multiples we could replace
foldr1 by foldl1?
-}

{-
Exercise B

The standard prelude function cycle :: [a] -> [a] takes a list xs and returns
a list consisting of an infinite number of repetitions of the elements of xs. If xs is
the empty list, then cycle [] returns an error message. For instance
	cycle "hallo" = "hallohallohallo...

Define cycle using a cyclic list. Ensure that your definition works on empty, finite
and infinite lists.
-}

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = ys where ys = xs ++ ys
