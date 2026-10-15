-- | Transformations that turn an unfamiliar recurrence into a familiar one.
--
-- * Domain: @S(n) = T(f(n))@, e.g. @t(k) = T(2^k)@. Solve for @t@, then map
--   the answer back with 'MyLib.Algorithms.Recurrences.Asymptotic.substituteLog'.
-- * Range: @S(n) = f(T(n))@, e.g. @U(n) = n · T(n)@.
-- * Difference: @ΔT(n) = T(n) - T(n - 1)@, which turns full-history
--   recurrences into limited-history ones.
module MyLib.Algorithms.Recurrences.Transformations
  ( difference
  , partialSums
  , harmonic
  , fullHistory
  ) where

import Data.Ratio ((%))
import MyLib.Algorithms.Recurrences.Annihilators (Scalar, apply, shift)

-- | The difference operator @Δ = E - 1@, applied to a sequence.
--
-- >>> difference [0, 1, 4, 9, 16 :: Rational]
-- [1 % 1,3 % 1,5 % 1,7 % 1]
difference :: Scalar a => [a] -> [a]
difference = apply (shift - 1)

-- | The inverse of 'difference', up to the starting value.
partialSums :: Num a => [a] -> [a]
partialSums = scanl1 (+)

-- | @H(n) = 1 + 1/2 + … + 1/n = Θ(log n)@.
harmonic :: Int -> Rational
harmonic n = sum [1 % toInteger i | i <- [1 .. n]]

-- | The full-history recurrence @T(n) = (2/n) Σ_{k<n} T(k) + f(n)@, with
-- @T(0) = 0@, computed with a running sum.
fullHistory :: (Rational -> Rational) -> [Rational]
fullHistory f = go 0 0
  where
    go :: Integer -> Rational -> [Rational]
    go 0 s = 0 : go 1 s
    go n s = t : go (n + 1) (s + t)
      where
        t = 2 * s / fromIntegral n + f (fromIntegral n)
