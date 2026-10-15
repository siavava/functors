-- | The Fibonacci numbers, bounded by induction and solved exactly by
-- annihilators, plus their cousin, the height-balanced tree recurrence.
module MyLib.Algorithms.Recurrences.Examples.Fibonacci
  ( fibonaccis
  , fibonacci
  , golden
  , fibonacciBounds
  , fibonacciRecurrence
  , heightBalancedRecurrence
  ) where

import MyLib.Algorithms.Recurrences.Annihilators (ClosedForm (..), Linear (..), Term (..), homogeneous)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | @F(n) = F(n - 1) + F(n - 2)@, @F(0) = 0@, @F(1) = 1@.
fibonaccis :: [Integer]
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (drop 1 fibonaccis)

-- |
-- >>> map fibonacci [0 .. 10]
-- [0,1,1,2,3,5,8,13,21,34,55]
fibonacci :: Int -> Integer
fibonacci = (fibonaccis !!)

-- | The golden ratio φ, the smallest @c@ with @c^2 ≥ c + 1@, and hence the
-- base that makes the inductive guess @F(n) ≤ α c^n@ go through.
golden :: Double
golden = (1 + sqrt 5) / 2

-- | The bounds @φ^(n-2) ≤ F(n) ≤ φ^(n-1)@ (for @n ≥ 1@) that the inductive
-- proof establishes, so @F(n) = Θ(φ^n)@.
fibonacciBounds :: Int -> (Double, Double)
fibonacciBounds n = (golden ^^ (n - 2), golden ^^ (n - 1))

-- | Annihilated by @(E - φ)(E - φ̂)@, which gives Binet's formula
-- @(φ^n - φ̂^n) / √5@.
--
-- >>> solve fibonacciRecurrence
-- Just 0.4472·1.618^n - 0.4472·(-0.618)^n
fibonacciRecurrence :: Linear
fibonacciRecurrence = homogeneous [1, 1] [0, 1]

-- | Height-balanced trees, @H(n) = H(n-1) + H(n-2) + 1@ with @H(-1) = 0@ and
-- @H(0) = 1@, shifted one place to start at zero.
--
-- >>> solve heightBalancedRecurrence
-- Just 1.1708·1.618^n - 1 - 0.1708·(-0.618)^n
heightBalancedRecurrence :: Linear
heightBalancedRecurrence = Linear [1, 1] (ClosedForm [Term 1 0 1]) [0, 1]
