-- | Steps 4 and 5 of the annihilator method: read the generic solution off
-- the factored annihilator, then fit its coefficients to the first few
-- values of the sequence.
module MyLib.Algorithms.Recurrences.Annihilators.Solve
  ( solveExact
  , solve
  ) where

import Data.Complex (Complex)
import Data.List (sortOn)
import Data.Ord (Down (..))
import MyLib.Algorithms.Recurrences.Annihilators.ClosedForm (ClosedForm (..), Term (..))
import MyLib.Algorithms.Recurrences.Annihilators.Linear (Linear, annihilator, values)
import MyLib.Algorithms.Recurrences.Annihilators.Operator (degree)
import MyLib.Algorithms.Recurrences.Annihilators.Roots (rationalRoots, roots)
import MyLib.Algorithms.Recurrences.Annihilators.Scalar (Scalar (..))

-- | Solve a recurrence exactly, if its annihilator splits over ℚ.
--
-- >>> import MyLib.Algorithms.Recurrences.Annihilators.Linear (homogeneous)
-- >>> solveExact (homogeneous [5, -8, 4] [1, 0, 0])
-- Just n·2^n - 3·2^n + 4
-- >>> solveExact (homogeneous [1, 1] [0, 1])
-- Nothing
solveExact :: Linear -> Maybe (ClosedForm Rational)
solveExact l = case rationalRoots (annihilator l) of
  (rs, rest) | degree rest == 0 -> solveWith rs (values l)
  _ -> Nothing

-- | Solve any recurrence, numerically where the roots are irrational.
--
-- >>> import MyLib.Algorithms.Recurrences.Annihilators.Linear (homogeneous)
-- >>> solve (homogeneous [1, 1] [0, 1])
-- Just 0.4472·1.618^n - 0.4472·(-0.618)^n
solve :: Linear -> Maybe (ClosedForm (Complex Double))
solve l = solveWith (roots (annihilator l)) (map fromRational (values l))

-- | Each factor @(E - r)^m@ contributes @n^d r^n@ for @d < m@. The terms
-- are ordered by dominance, so the first one printed is the one that
-- matters asymptotically.
solveWith :: Scalar a => [(a, Int)] -> [a] -> Maybe (ClosedForm a)
solveWith rs vs = do
  coeffs <- gaussian rows (take k vs)
  pure (ClosedForm [Term c d r | (c, (d, r)) <- zip coeffs basis, not (isZero c)])
  where
    basis = sortOn (\(d, r) -> Down (size r, d)) [(d, r) | (r, m) <- rs, d <- [0 .. m - 1]]
    k = length basis
    rows = [[fromIntegral n ^ d * r ^ n | (d, r) <- basis] | n <- [0 .. k - 1 :: Int]]

-- | Solve a square linear system by Gaussian elimination with partial
-- pivoting. 'Nothing' if it is singular.
gaussian :: Scalar a => [[a]] -> [a] -> Maybe [a]
gaussian a b = backSubstitute <$> eliminate (zipWith (\row y -> row ++ [y]) a b)
  where
    eliminate [] = Just []
    eliminate rows
      | isZero p = Nothing
      | otherwise = (pivot :) <$> eliminate (map (drop 1 . reduce) others)
      where
        i = snd (maximum [(size (lead r), j) | (j, r) <- zip [0 :: Int ..] rows])
        pivot = rows !! i
        others = [r | (j, r) <- zip [0 ..] rows, j /= i]
        p = lead pivot
        reduce r = zipWith (\x y -> x - (lead r / p) * y) r pivot
    backSubstitute [] = []
    backSubstitute (row : rest) = case row of
      c : cs -> (last cs - sum (zipWith (*) (init cs) xs)) / c : xs
      [] -> xs
      where
        xs = backSubstitute rest
    lead r = case r of
      x : _ -> x
      [] -> 0
