-- | Closed forms @Σ αᵢ · n^dᵢ · rᵢ^n@: the shape of every solution to a
-- linear recurrence, and exactly the functions annihilators kill.
module MyLib.Algorithms.Recurrences.Annihilators.ClosedForm
  ( Term (..)
  , ClosedForm (..)
  , evaluate
  , annihilatorOf
  , asymptotic
  ) where

import Data.List (intercalate, nub)
import MyLib.Algorithms.Recurrences.Annihilators.Operator (Operator, operator, shift)
import MyLib.Algorithms.Recurrences.Annihilators.Scalar (Scalar (..), parenthesise, renderSum)
import MyLib.Algorithms.Recurrences.Asymptotic (Theta (..), constant)

-- | A single term @α · n^d · r^n@.
data Term a = Term
  { coefficient :: a
  , power       :: Int
  , root        :: a
  }
  deriving (Eq)

-- | A sum of polynomial-times-exponential terms.
--
-- >>> ClosedForm [Term 1 1 2, Term (-3) 0 2, Term 4 0 1] :: ClosedForm Rational
-- n·2^n - 3·2^n + 4
newtype ClosedForm a = ClosedForm [Term a]
  deriving (Eq)

instance Scalar a => Show (ClosedForm a) where
  show (ClosedForm ts) = renderSum "·" [(c, symbol d r) | Term c d r <- ts]
    where
      symbol d r = intercalate "·" (filter (not . null) [poly d, expo r])
      poly 0 = ""
      poly 1 = "n"
      poly d = "n^" ++ show d
      expo r
        | isZero (r - 1) = ""
        | otherwise = parenthesise "-+/" (render r) ++ "^n"

-- | Evaluate a closed form at @n@.
evaluate :: Num a => ClosedForm a -> Int -> a
evaluate (ClosedForm ts) n = sum [c * fromIntegral n ^ d * r ^ n | Term c d r <- ts]

-- | The minimal annihilator of a closed form: @(E - r)^(d + 1)@ for each
-- root @r@ whose highest power of @n@ is @d@.
--
-- >>> annihilatorOf (ClosedForm [Term 1 0 2, Term (-1) 2 1] :: ClosedForm Rational)
-- E^4 - 5E^3 + 9E^2 - 7E + 2
annihilatorOf :: Scalar a => ClosedForm a -> Operator a
annihilatorOf (ClosedForm ts) =
  product [(shift - operator [r]) ^ (1 + maximum [d | Term _ d r' <- live, r' == r]) | r <- nub (map root live)]
  where
    live = filter (not . isZero . coefficient) ts

-- | The growth class of a closed form: its dominant term, i.e. the
-- largest root, then the highest power of @n@.
--
-- >>> asymptotic (ClosedForm [Term 1 1 2, Term (-3) 0 2, Term 4 0 1] :: ClosedForm Rational)
-- Θ(n 2^n)
asymptotic :: Scalar a => ClosedForm a -> Theta
asymptotic (ClosedForm ts) = case [(size r, d) | Term c d r <- ts, not (isZero c)] of
  [] -> constant
  xs -> let (r, d) = maximum xs in Theta r (fromIntegral d) 0 0
