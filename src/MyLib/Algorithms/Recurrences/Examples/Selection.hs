-- | Selection: both recurrences have recursion trees whose level sums
-- shrink geometrically, so the root's linear work dominates.
module MyLib.Algorithms.Recurrences.Examples.Selection
  ( randomizedSelection
  , deterministicSelection
  ) where

import MyLib.Algorithms.Recurrences.Trees (DivideAndConquer, standard)

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | Randomized selection: @T(n) = T(3n/4) + n@. A single path whose values
-- shrink by @3/4@ each level: @Θ(n)@.
--
-- >>> series (recursionTree randomizedSelection 4096)
-- Decreasing
randomizedSelection :: DivideAndConquer
randomizedSelection = standard [(1, 4 / 3)] id

-- | Deterministic selection: @T(n) = T(n/5) + T(7n/10) + n@. Complete
-- levels shrink by @9/10@: @Θ(n)@. Its Akra–Bazzi exponent has no closed
-- form, but it is below 1, which is all that matters.
--
-- >>> akraBazzi [(1, 5), (1, 10/7)] 1 0
-- Θ(n)
deterministicSelection :: DivideAndConquer
deterministicSelection = standard [(1, 5), (1, 10 / 7)] id
