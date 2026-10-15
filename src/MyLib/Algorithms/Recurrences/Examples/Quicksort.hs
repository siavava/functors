-- | Randomized quicksort: as a lopsided recursion tree, by Akra–Bazzi,
-- and exactly, by turning its full-history recurrence into a limited one.
module MyLib.Algorithms.Recurrences.Examples.Quicksort
  ( randomizedQuicksort
  , quicksortFullHistory
  , quicksortLimitedHistory
  , quicksortClosed
  , quicksortComparisons
  , averageComparisons
  , expectedComparisons
  , expectedComparisonsClosed
  ) where

import Data.List (partition, permutations)
import Data.Ratio ((%))
import MyLib.Algorithms.Recurrences.Transformations (fullHistory, harmonic)
import MyLib.Algorithms.Recurrences.Trees (DivideAndConquer, standard)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | The good-split model @T(n) = T(3n/4) + T(n/4) + n@. Complete levels sum
-- to @n@ and leaves lie between depths @log₄ n@ and @log_{4/3} n@.
--
-- >>> leafDepths (recursionTree randomizedQuicksort 4096)
-- (6,29)
-- >>> akraBazzi [(1, 4/3), (1, 4)] 1 0
-- Θ(n log n)
randomizedQuicksort :: DivideAndConquer
randomizedQuicksort = standard [(1, 4 / 3), (1, 4)] id

-- | The exact expectation @T(n) = (2/n) Σ_{k<n} T(k) + n@.
quicksortFullHistory :: [Rational]
quicksortFullHistory = fullHistory id

-- | The same function after multiplying by @n@, shifting, and subtracting:
-- @T(n) = ((n + 1)/n) T(n-1) + 2 - 1/n@.
quicksortLimitedHistory :: [Rational]
quicksortLimitedHistory = scanl step 0 [1 ..]
  where
    step t n = (n + 1) / n * t + 2 - 1 / n

-- | Dividing by @n + 1@ leaves @t(n) = t(n-1) + 2/(n+1) - 1/(n(n+1))@,
-- which sums to @T(n) = 2(n + 1) H(n + 1) - 3n - 2 = Θ(n log n)@.
quicksortClosed :: Int -> Rational
quicksortClosed n = 2 * fromIntegral (n + 1) * harmonic (n + 1) - 3 * fromIntegral n - 2

-- | Comparisons made by quicksort with the first element as pivot.
quicksortComparisons :: Ord a => [a] -> Int
quicksortComparisons [] = 0
quicksortComparisons (p : xs) = length xs + quicksortComparisons smaller + quicksortComparisons larger
  where
    (smaller, larger) = partition (< p) xs

-- | Comparisons averaged over every permutation of @n@ distinct keys,
-- found by brute force.
averageComparisons :: Int -> Rational
averageComparisons n = toInteger (sum (map quicksortComparisons ps)) % toInteger (length ps)
  where
    ps = permutations [1 .. n]

-- | The recurrence that average obeys: @C(n) = (2/n) Σ_{k<n} C(k) + n - 1@.
--
-- >>> take 5 expectedComparisons
-- [0 % 1,0 % 1,1 % 1,8 % 3,29 % 6]
expectedComparisons :: [Rational]
expectedComparisons = fullHistory (subtract 1)

-- | Its solution, @C(n) = 2(n + 1) H(n) - 4n@.
expectedComparisonsClosed :: Int -> Rational
expectedComparisonsClosed n = 2 * fromIntegral (n + 1) * harmonic n - 4 * fromIntegral n
