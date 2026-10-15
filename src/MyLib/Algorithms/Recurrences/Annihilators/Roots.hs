-- | Factoring an annihilator into @(E - r)@ factors: exactly over ℚ where
-- possible, numerically over ℂ for whatever is left.
module MyLib.Algorithms.Recurrences.Annihilators.Roots
  ( rationalRoots
  , roots
  ) where

import Data.Complex (Complex (..))
import Data.List (nub)
import Data.Ratio (denominator, numerator)
import MyLib.Algorithms.Recurrences.Annihilators.Operator (Operator (..), deflate, degree, evalAt)

-- | The rational roots of an operator with their multiplicities (by the
-- rational root theorem), and the factor that does not split over ℚ.
--
-- >>> rationalRoots (Operator [-4, 8, -5, 1])
-- ([(1 % 1,1),(2 % 1,2)],1)
-- >>> rationalRoots (Operator [-1, -1, 1])
-- ([],E^2 - E - 1)
rationalRoots :: Operator Rational -> ([(Rational, Int)], Operator Rational)
rationalRoots p = foldl extract ([(0, z) | z > 0], q) (candidates q)
  where
    (z, q) = case p of
      Operator cs -> let (zs, rest) = span (== 0) cs in (length zs, Operator rest)
    extract (found, r) c = case multiplicity c r of
      (0, _) -> (found, r)
      (m, r') -> (found ++ [(c, m)], r')

multiplicity :: Rational -> Operator Rational -> (Int, Operator Rational)
multiplicity c r
  | degree r > 0 && evalAt c r == 0 = let (m, r') = multiplicity c (deflate c r) in (m + 1, r')
  | otherwise = (0, r)

-- | Candidate roots @±p/q@ with @p@ dividing the constant term and @q@ the
-- leading coefficient, after clearing denominators.
candidates :: Operator Rational -> [Rational]
candidates (Operator cs) = case map numerator scaled of
  [] -> []
  ints@(a0 : _) ->
    nub [s * fromIntegral d / fromIntegral e | d <- divisors a0, e <- divisors (last ints), s <- [1, -1]]
  where
    scaled = map (* fromIntegral (foldr (lcm . denominator) 1 cs)) cs
    divisors n = [d | d <- [1 .. abs n], n `mod` d == 0]

-- | Every root with its multiplicity. Rational roots are found exactly.
-- The rest come from the Durand–Kerner iteration, which assumes the
-- leftover factor has distinct roots.
roots :: Operator Rational -> [(Complex Double, Int)]
roots p = [(fromRational r, m) | (r, m) <- exact] ++ [(clean z, 1) | z <- durandKerner rest]
  where
    (exact, Operator rest) = rationalRoots p
    clean (x :+ y) = if abs y < 1e-9 then x :+ 0 else x :+ y

durandKerner :: [Rational] -> [Complex Double]
durandKerner cs
  | n < 1 = []
  | otherwise = iterate step start !! 500
  where
    n = length cs - 1
    monic = Operator (map (\c -> fromRational (c / last cs)) cs) :: Operator (Complex Double)
    start = [(0.4 :+ 0.9) ^ k | k <- [0 .. n - 1]]
    step zs =
      [ z - evalAt z monic / product [z - w | (j, w) <- zip [0 :: Int ..] zs, j /= i]
      | (i, z) <- zip [0 ..] zs
      ]
