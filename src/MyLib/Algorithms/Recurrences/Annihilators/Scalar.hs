-- | The fields linear recurrences are solved over, and how their elements
-- print.
module MyLib.Algorithms.Recurrences.Annihilators.Scalar
  ( Scalar (..)
  , renderSum
  , parenthesise
  ) where

import Data.Complex (Complex (..), magnitude)
import Data.Ratio (denominator, numerator)
import MyLib.Algorithms.Recurrences.Asymptotic (showNum)

-- | Exact 'Rational's when the annihilator splits over ℚ, and
-- @'Complex' 'Double'@ when it doesn't.
class (Eq a, Fractional a) => Scalar a where
  size     :: a -> Double
  isZero   :: a -> Bool
  negative :: a -> Bool
  render   :: a -> String

instance Scalar Rational where
  size = abs . fromRational
  isZero = (== 0)
  negative = (< 0)
  render q
    | denominator q == 1 = show (numerator q)
    | otherwise = show (numerator q) ++ "/" ++ show (denominator q)

instance Scalar Double where
  size = abs
  isZero x = abs x < 1e-9
  negative = (< 0)
  render = showNum

instance Scalar (Complex Double) where
  size = magnitude
  isZero z = magnitude z < 1e-9
  negative (x :+ y) = abs y < 1e-9 && x < 0
  render (x :+ y)
    | abs y < 1e-9 = showNum x
    | otherwise = showNum x ++ (if y < 0 then "-" else "+") ++ showNum (abs y) ++ "i"

-- | Render @Σ cᵢ · sᵢ@, folding signs into the joins and dropping unit
-- coefficients. The separator goes between a coefficient and its symbol.
renderSum :: Scalar a => String -> [(a, String)] -> String
renderSum _ [] = "0"
renderSum sep (t : ts) = concat (lead t : map rest ts)
  where
    lead (c, s) = if negative c then "-" ++ body (negate c) s else body c s
    rest (c, s) = if negative c then " - " ++ body (negate c) s else " + " ++ body c s
    body c "" = render c
    body c s
      | isZero (c - 1) = s
      | otherwise = parenthesise "-+" (render c) ++ sep ++ s

-- | Wrap a rendered number in parentheses if any of the given characters
-- appear in it.
parenthesise :: String -> String -> String
parenthesise cs s
  | any (`elem` cs) s = "(" ++ s ++ ")"
  | otherwise = s
