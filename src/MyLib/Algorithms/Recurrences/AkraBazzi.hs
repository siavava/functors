-- | Closed-form theorems for divide-and-conquer recurrences: the Master
-- theorem, and the "nuclear bomb" that generalises it, the Akra–Bazzi
-- theorem.
module MyLib.Algorithms.Recurrences.AkraBazzi
  ( master
  , akraBazzi
  , akraBazziExponent
  ) where

import MyLib.Algorithms.Recurrences.Asymptotic (Theta (..), near)

-- | The Master theorem for @T(n) = a T(n/b) + Θ(n^c (log n)^k)@, as the
-- one-term case of 'akraBazzi'.
--
-- >>> master 2 2 1 0
-- Θ(n log n)
-- >>> master 3 2 1 0
-- Θ(n^1.585)
-- >>> master 2 2 1 (-1)
-- Θ(n log log n)
-- >>> master 4 2 1 1
-- Θ(n^2)
master :: Double -> Double -> Double -> Double -> Theta
master a b = akraBazzi [(a, b)]

-- | The Akra–Bazzi theorem for @T(n) = Σ aᵢ T(n/bᵢ) + Θ(n^c (log n)^k)@:
--
-- @T(n) = Θ(n^ρ (1 + ∫₁ⁿ f(u) / u^(ρ+1) du))@
--
-- where @Σ aᵢ / bᵢ^ρ = 1@. The integral is evaluated symbolically for
-- polylogarithmic @f@.
--
-- >>> akraBazzi [(1, 4/3), (1, 4)] 1 0
-- Θ(n log n)
-- >>> akraBazzi [(1, 5), (1, 10/7)] 1 0
-- Θ(n)
-- >>> akraBazzi [(1/4, 4), (3/4, 4/3)] 0 0
-- Θ(log n)
-- >>> akraBazzi [(1, 2), (1, 4)] 0 0
-- Θ(n^0.6942)
akraBazzi :: [(Double, Double)] -> Double -> Double -> Theta
akraBazzi terms c k
  | near c rho = atCritical
  | c < rho = Theta 1 rho 0 0
  | otherwise = Theta 1 c k 0
  where
    rho = akraBazziExponent terms
    -- ∫ (log u)^k / u du: log^(k+1) n, log log n, or a constant.
    atCritical
      | near k (-1) = Theta 1 c 0 1
      | k > -1 = Theta 1 c (k + 1) 0
      | otherwise = Theta 1 c 0 0

-- | The unique real @ρ@ with @Σ aᵢ / bᵢ^ρ = 1@ (needs @aᵢ > 0@, @bᵢ > 1@).
-- The sum is strictly decreasing in @ρ@, so bisection finds it.
--
-- >>> akraBazziExponent [(1, 5), (1, 10/7)]
-- 0.8397803044678207
akraBazziExponent :: [(Double, Double)] -> Double
akraBazziExponent terms = snap (bisect lo hi (200 :: Int))
  where
    g rho = sum [a / b ** rho | (a, b) <- terms]
    lo = until (\x -> g x >= 1) (* 2) (-1)
    hi = until (\x -> g x <= 1) (* 2) 1
    bisect l h 0 = (l + h) / 2
    bisect l h i
      | g m > 1 = bisect m h (i - 1)
      | otherwise = bisect l m (i - 1)
      where
        m = (l + h) / 2
    snap x = let r = fromIntegral (round x :: Integer) in if near x r then r else x
