-- | Recursion trees for divide-and-conquer recurrences.
--
-- @T(n)@ is the sum of every value in its recursion tree. Summing level
-- by level usually gives a geometric series, and only its largest term
-- matters.
module MyLib.Algorithms.Recurrences.Trees
  ( -- * Divide-and-conquer recurrences
    DivideAndConquer (..)
  , standard
    -- * Recursion trees
  , RecursionTree (..)
  , recursionTree
  , solveTree
  , levels
  , levelSums
  , completeLevelSums
  , leafDepths
    -- * Reading the level sums
  , Series (..)
  , series
  ) where

import MyLib.Algorithms.Recurrences.Asymptotic (near)

-- | A recurrence @T(n) = Σ wᵢ · T(nᵢ) + f(n)@, where the weighted
-- subproblems @(wᵢ, nᵢ)@ may depend on @n@.
data DivideAndConquer = DivideAndConquer
  { split    :: Double -> [(Double, Double)]
    -- ^ The @(weight, size)@ of each subproblem of a problem of size @n@.
  , cost     :: Double -> Double
    -- ^ @f(n)@, the work done outside the recursive calls.
  , baseSize :: Double
    -- ^ Problems this size or smaller are base cases costing @1@.
  }

-- | The standard form @T(n) = Σ aᵢ T(n / bᵢ) + f(n)@, with @T(n) = 1@ for
-- @n ≤ 1@.
standard :: [(Double, Double)] -> (Double -> Double) -> DivideAndConquer
standard terms f = DivideAndConquer (\n -> [(a, n / b) | (a, b) <- terms]) f 1

-- | A weighted recursion tree. Each node holds its subproblem size and its
-- contribution to the total. Rather than @a@ identical children for
-- @a · T(n/b)@, a node has one child of weight @a@. That is what makes a
-- "quarter of a child" in @¼ T(n/4)@ meaningful.
data RecursionTree = Node
  { nodeSize  :: Double
  , nodeValue :: Double
  , subtrees  :: [RecursionTree]
  }

-- | Unfold the recursion tree of a problem of size @n@.
recursionTree :: DivideAndConquer -> Double -> RecursionTree
recursionTree dc = go 1
  where
    go w n
      | n <= baseSize dc = Node n w []
      | otherwise = Node n (w * cost dc n) [go (w * a) m | (a, m) <- split dc n]

-- | @T(n)@, as the sum of its recursion tree.
--
-- >>> solveTree (standard [(2, 2)] id) 1024
-- 11264.0
solveTree :: DivideAndConquer -> Double -> Double
solveTree dc = sum . levelSums . recursionTree dc

-- | The nodes of a tree, level by level from the root.
levels :: RecursionTree -> [[RecursionTree]]
levels t = takeWhile (not . null) (iterate (concatMap subtrees) [t])

-- | The level-by-level sum @Σ aⁱ f(n/bⁱ)@.
--
-- >>> levelSums (recursionTree (standard [(3, 2)] id) 16)
-- [16.0,24.0,36.0,54.0,81.0]
levelSums :: RecursionTree -> [Double]
levelSums = map (sum . map nodeValue) . levels

-- | Sums of the levels strictly above the shallowest leaf. For lopsided
-- trees these are the levels where the pattern is clean.
completeLevelSums :: RecursionTree -> [Double]
completeLevelSums t = take (fst (leafDepths t)) (levelSums t)

-- | Depths of the shallowest and the deepest leaf.
leafDepths :: RecursionTree -> (Int, Int)
leafDepths t = (minimum depths, maximum depths)
  where
    depths = go 0 t
    go d (Node _ _ []) = [d]
    go d (Node _ _ cs) = concatMap (go (d + 1)) cs

-- | The shape of the level sums.
data Series
  = Decreasing  -- ^ Dominated by the root: @T(n) = Θ(f(n))@.
  | Level       -- ^ Every level equal: @T(n) = Θ(f(n) · depth)@.
  | Increasing  -- ^ Dominated by the leaves.
  | Mixed       -- ^ None of the above; sum it by hand.
  deriving (Eq, Show)

-- | Classify the complete level sums of a tree.
--
-- >>> [series (recursionTree (standard [(a, 2)] id) 4096) | a <- [1, 2, 3]]
-- [Decreasing,Level,Increasing]
series :: RecursionTree -> Series
series t
  | null rs = Mixed
  | all (near 1) rs = Level
  | all (< 1) rs = Decreasing
  | all (> 1) rs = Increasing
  | otherwise = Mixed
  where
    xs = completeLevelSums t
    rs = zipWith (/) (drop 1 xs) xs
