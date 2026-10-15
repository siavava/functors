-- | Recurrences from the notes that illustrate a method rather than
-- analyse a particular algorithm.
module MyLib.Algorithms.Recurrences.Examples.Assorted
  ( -- * One function, many recurrences
    identityA
  , identityB
  , identityC
    -- * Recursion trees
  , karatsuba
  , quadNLogN
  , rootSplit
    -- * Linear recurrences
  , fiveFoldRecurrence
  , cubicRecurrence
  , mixedRecurrence
  ) where

import MyLib.Algorithms.Recurrences.Annihilators (ClosedForm (..), Linear (..), Term (..), homogeneous)
import MyLib.Algorithms.Recurrences.Guess (memoFix)
import MyLib.Algorithms.Recurrences.Trees (DivideAndConquer (..), standard)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | @f(n) = f(n - 1) + 1@.
--
-- >>> map identityA [0 .. 5]
-- [0,1,2,3,4,5]
identityA :: Int -> Int
identityA = memoFix go
  where
    go _ 0 = 0
    go f n = f (n - 1) + 1

-- | @f(n) = f(⌊n/2⌋) + f(⌈n/2⌉)@ with two base cases.
identityB :: Int -> Int
identityB = memoFix go
  where
    go _ 0 = 0
    go _ 1 = 1
    go f n = f (n `div` 2) + f ((n + 1) `div` 2)

-- | @f(n) = 2 f(n/2)@ for even @n@, @f(n - 1) + 1@ for odd @n@.
identityC :: Int -> Int
identityC = memoFix go
  where
    go _ 0 = 0
    go f n
      | even n = 2 * f (n `div` 2)
      | otherwise = f (n - 1) + 1

-- | Karatsuba multiplication: @T(n) = 3T(n/2) + n@. An increasing series,
-- dominated by its @n^(lg 3)@ leaves.
--
-- >>> levelSums (recursionTree karatsuba 16)
-- [16.0,24.0,36.0,54.0,81.0]
karatsuba :: DivideAndConquer
karatsuba = standard [(3, 2)] id

-- | @T(n) = 4T(n/2) + n lg n@. Not quite geometric (its level ratios
-- flatten to 1 near the leaves), but still dominated by them: @Θ(n²)@.
--
-- >>> master 4 2 1 1
-- Θ(n^2)
quadNLogN :: DivideAndConquer
quadNLogN = standard [(4, 2)] (\n -> n * logBase 2 n)

-- | The ugly @T(n) = √n · T(√n) + n@. Every level sums to @n@ and there are
-- @lg lg n@ levels, so @T(n) = Θ(n log log n)@. Square roots never reach
-- 1, so the recursion stops at 2.
--
-- >>> series (recursionTree rootSplit 65536)
-- Level
rootSplit :: DivideAndConquer
rootSplit = DivideAndConquer (\n -> [(sqrt n, sqrt n)]) id 2

-- | @r(n) = 5 r(n-1)@, @r(0) = 3@.
--
-- >>> solveExact fiveFoldRecurrence
-- Just 3·5^n
fiveFoldRecurrence :: Linear
fiveFoldRecurrence = homogeneous [5] [3]

-- | @T(n) = 5T(n-1) - 8T(n-2) + 4T(n-3)@, @T(0) = 1@, @T(1) = T(2) = 0@, with
-- annihilator @(E - 2)²(E - 1)@. (The notes misprint the first coefficient
-- as 3. Their annihilator and solution both need 5.)
--
-- >>> solveExact cubicRecurrence
-- Just n·2^n - 3·2^n + 4
cubicRecurrence :: Linear
cubicRecurrence = homogeneous [5, -8, 4] [1, 0, 0]

-- | @T(n) = T(n-1) + 2T(n-2) + 2^n - n²@, with annihilator
-- @(E - 2)²(E - 1)³(E + 1)@, so @T(n) = Θ(n 2^n)@ for almost every choice
-- of base cases.
--
-- >>> asymptotic <$> solveExact mixedRecurrence
-- Just Θ(n 2^n)
mixedRecurrence :: Linear
mixedRecurrence = Linear [1, 2] (ClosedForm [Term 1 0 2, Term (-1) 2 1]) [0, 0]
