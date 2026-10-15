-- | The Tower of Hanoi, @T(n) = 2T(n - 1) + 1@ with @T(0) = 0@, solved by
-- guessing, by unrolling, and by annihilators.
module MyLib.Algorithms.Recurrences.Examples.Hanoi
  ( hanoi
  , hanoiClosed
  , hanoiUnrolled
  , hanoiMoves
  , hanoiRecurrence
  ) where

import MyLib.Algorithms.Recurrences.Annihilators (ClosedForm (..), Linear (..), Term (..))

-- $setup
-- >>> import MyLib.Algorithms.Recurrences

-- | Moves needed for @n@ disks, straight from the recurrence.
--
-- >>> map hanoi [0 .. 6]
-- [0,1,3,7,15,31,63]
hanoi :: Int -> Integer
hanoi 0 = 0
hanoi n = 2 * hanoi (n - 1) + 1

-- | The guess suggested by the first few values: @T(n) = 2^n - 1@.
hanoiClosed :: Int -> Integer
hanoiClosed n = 2 ^ n - 1

-- | The recurrence unrolled @k@ times: @T(n) = 2^k T(n - k) + 2^k - 1@.
-- Unrolling all the way (@k = n@) lands on the base case and the closed form.
hanoiUnrolled :: Int -> Int -> Integer
hanoiUnrolled k n = 2 ^ k * hanoi (n - k) + 2 ^ k - 1

-- | The moves themselves, as @(from, to)@ pegs, moving @n@ disks from peg 1
-- to peg 3. The algorithm's shape is exactly the recurrence's shape.
--
-- >>> hanoiMoves 2
-- [(1,2),(1,3),(2,3)]
hanoiMoves :: Int -> [(Int, Int)]
hanoiMoves n = go n 1 3 2
  where
    go 0 _ _ _ = []
    go m from to via = go (m - 1) from via to ++ [(from, to)] ++ go (m - 1) via to from

-- | The first non-homogeneous recurrence: @(E - 2)@ leaves a residue of
-- @1@, which @(E - 1)@ then annihilates.
--
-- >>> solveExact hanoiRecurrence
-- Just 2^n - 1
hanoiRecurrence :: Linear
hanoiRecurrence = Linear [2] (ClosedForm [Term 1 0 1]) [0]
