{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

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

ANSWER: No, since foldl1 f xs = undefined for any infinite list xs.
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

{-
Exercise C

The fibonacci function is defined by
	fib 0 = 0
	fib 1 = 1
	fib n = fib (n-1) + fib (n-2)

Write down a one-line definition of the list fibs that produces the infinite list of
Fibonacci numbers.
-}

fibs :: [Integer]
-- fibs = xs where xs = 0 : 1 : [xs !! 0 + xs !! 1]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
Exercise D

A well-known problem, due to the mathematician W.R. Hamming, is to write a
program that produces an infinite list of numbers with the following properties:
(i) the list is in strictly increasing order;
(ii) the list begins with the number 1;
(iii) if the list contains the number x, then it also contains the numbers 2x, 3x and 5x;
(iv) the list contains no other numbers.
Thus, the required list begins with the numbers
	1,2,3,4,5,6,8,9,10,12,15,16,...

Write a definition of hamming that produces this list.
-}

hamming :: [Integer]
hamming = 1 : mergeAll [map (2 *) hamming, map (3 *) hamming, map (5 *) hamming]

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll = foldr1 xmerge

xmerge :: (Ord a) => [a] -> [a] -> [a]
xmerge (x : xs) ys = x : merge xs ys

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x == y = x : merge xs ys
  | x > y = y : merge (x : xs) ys
