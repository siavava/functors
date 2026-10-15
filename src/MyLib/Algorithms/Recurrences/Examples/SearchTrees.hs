-- | Search-tree recurrences that defeat plain recursion trees: random
-- search trees (fractional children) and ham-sandwich trees (bounds too
-- loose to be useful).
module MyLib.Algorithms.Recurrences.Examples.SearchTrees
  ( randomSearchTree
  , randomSearchTreeRange
  , hamSandwich
  , hamSandwichExact
  , hamSandwichDomain
  ) where

import MyLib.Algorithms.Recurrences.Annihilators (ClosedForm (..), Linear (..), Term (..))
import MyLib.Algorithms.Recurrences.Trees (DivideAndConquer, standard)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | Random search trees: @T(n) = ¼ T(n/4) + ¾ T(3n/4) + 1@. Weighted
-- complete levels sum to exactly 1: @Θ(log n)@.
--
-- >>> akraBazzi [(1/4, 4), (3/4, 4/3)] 0 0
-- Θ(log n)
randomSearchTree :: DivideAndConquer
randomSearchTree = standard [(1 / 4, 4), (3 / 4, 4 / 3)] (const 1)

-- | The range transformation @U(n) = n · T(n)@ removes the weights:
-- @U(n) = U(n/4) + U(3n/4) + n@. That is @Θ(n log n)@, so @T(n) = Θ(log n)@.
randomSearchTreeRange :: DivideAndConquer
randomSearchTreeRange = standard [(1, 4), (1, 4 / 3)] id

-- | Ham-sandwich trees: @T(n) = T(n/2) + T(n/4) + 1@. Recursion trees only
-- give @√n ≲ T(n) ≲ n@.
--
-- >>> akraBazzi [(1, 2), (1, 4)] 0 0
-- Θ(n^0.6942)
hamSandwich :: DivideAndConquer
hamSandwich = standard [(1, 2), (1, 4)] (const 1)

-- | The same recurrence on the integers: @T(n) = T(⌊n/2⌋) + T(⌊n/4⌋) + 1@,
-- with @T(n) = 1@ for @n < 2@.
hamSandwichExact :: Integer -> Integer
hamSandwichExact n
  | n < 2 = 1
  | otherwise = hamSandwichExact (n `div` 2) + hamSandwichExact (n `div` 4) + 1

-- | The domain transformation @t(k) = T(2^k)@ gives the Fibonacci-like
-- @t(k) = t(k-1) + t(k-2) + 1@, so @t(k) = Θ(φ^k)@ and
-- @T(n) = Θ(φ^(lg n)) = Θ(n^(lg φ))@.
--
-- >>> substituteLog . asymptotic <$> solve hamSandwichDomain
-- Just Θ(n^0.6942)
hamSandwichDomain :: Linear
hamSandwichDomain = Linear [1, 1] (ClosedForm [Term 1 0 1]) [1, 3]
