-- | Mergesort, solved four ways: exactly with the floors and ceilings left
-- in, by recursion tree, by the Master theorem, and by a domain
-- transformation into a linear recurrence.
module MyLib.Algorithms.Recurrences.Examples.Mergesort
  ( mergeSortCounting
  , mergeSortWorstCase
  , mergeSortWorstCaseClosed
  , mergesort
  , mergesortOverLog
  , mergesortDomain
  ) where

import MyLib.Algorithms.Recurrences.Annihilators (ClosedForm (..), Linear (..), Term (..))
import MyLib.Algorithms.Recurrences.Trees (DivideAndConquer, standard)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | Mergesort, also reporting how many comparisons it made.
--
-- >>> mergeSortCounting [3, 1, 2 :: Int]
-- ([1,2,3],3)
mergeSortCounting :: Ord a => [a] -> ([a], Int)
mergeSortCounting xs
  | n <= 1 = (xs, 0)
  | otherwise = (merged, cl + cr + cm)
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) xs
    (sl, cl) = mergeSortCounting l
    (sr, cr) = mergeSortCounting r
    (merged, cm) = merge sl sr

merge :: Ord a => [a] -> [a] -> ([a], Int)
merge [] ys = (ys, 0)
merge xs [] = (xs, 0)
merge (x : xs) (y : ys)
  | x <= y = let (zs, c) = merge xs (y : ys) in (x : zs, c + 1)
  | otherwise = let (zs, c) = merge (x : xs) ys in (y : zs, c + 1)

-- | Worst-case comparisons of 'mergeSortCounting', with the floors and
-- ceilings left in: @W(n) = W(⌈n/2⌉) + W(⌊n/2⌋) + n - 1@, @W(1) = 0@.
mergeSortWorstCase :: Int -> Integer
mergeSortWorstCase n
  | n <= 1 = 0
  | otherwise =
      mergeSortWorstCase ((n + 1) `div` 2) + mergeSortWorstCase (n `div` 2) + toInteger n - 1

-- | The exact solution @W(n) = n⌈lg n⌉ - 2^⌈lg n⌉ + 1@. The floors and
-- ceilings don't stop it from being @Θ(n log n)@.
mergeSortWorstCaseClosed :: Int -> Integer
mergeSortWorstCaseClosed n = toInteger n * toInteger c - 2 ^ c + 1
  where
    c = length (takeWhile (< n) (iterate (* 2) 1))

-- | Simplified mergesort, @T(n) = 2T(n/2) + n@. Every level of the
-- recursion tree sums to @n@.
--
-- >>> solveTree mergesort 1024
-- 11264.0
mergesort :: DivideAndConquer
mergesort = standard [(2, 2)] id

-- | @T(n) = 2T(n/2) + n / lg n@. Level @i@ sums to @n / (lg n - i)@, a
-- harmonic series, so @T(n) = Θ(n log log n)@.
--
-- >>> master 2 2 1 (-1)
-- Θ(n log log n)
mergesortOverLog :: DivideAndConquer
mergesortOverLog = standard [(2, 2)] (\n -> n / logBase 2 n)

-- | @t(k) = T(2^k)@ satisfies @t(k) = 2t(k-1) + 2^k@, so
-- @t(k) = (k + 1) 2^k@ and @T(n) = n lg n + n@. (Closed forms always
-- print their variable as @n@; here it stands for @k@.)
--
-- >>> solveExact mergesortDomain
-- Just n·2^n + 2^n
-- >>> substituteLog . asymptotic <$> solveExact mergesortDomain
-- Just Θ(n log n)
mergesortDomain :: Linear
mergesortDomain = Linear [2] (ClosedForm [Term 1 0 2]) [1]
