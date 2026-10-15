-- | Operators on sequences, built from sums, scalings and the shift
-- @E f(n) = f(n + 1)@. They behave exactly like polynomials in @E@, so
-- here they /are/ polynomials, with a 'Num' instance.
module MyLib.Algorithms.Recurrences.Annihilators.Operator
  ( Operator (..)
  , operator
  , shift
  , degree
  , apply
  , annihilates
  , evalAt
  , deflate
  ) where

import MyLib.Algorithms.Recurrences.Annihilators.Scalar (Scalar (..), renderSum)

-- | A polynomial in the shift operator @E@, coefficients lowest power first.
newtype Operator a = Operator [a]
  deriving (Eq)

-- | Build an operator, dropping vanishing leading coefficients.
operator :: Scalar a => [a] -> Operator a
operator = Operator . reverse . dropWhile isZero . reverse

-- | The shift operator @E@.
shift :: Scalar a => Operator a
shift = Operator [0, 1]

-- | Operators multiply like polynomials. Composition commutes, so the
-- factors of an annihilator can be applied in any order.
--
-- >>> (shift - 1) * (shift - 2) :: Operator Rational
-- E^2 - 3E + 2
instance Scalar a => Num (Operator a) where
  Operator xs + Operator ys = operator (addPoly xs ys)
  Operator xs * Operator ys = operator (foldr (\x acc -> addPoly (map (x *) ys) (0 : acc)) [] xs)
  negate (Operator xs) = Operator (map negate xs)
  fromInteger n = operator [fromInteger n]
  abs = error "Operator: abs is not meaningful"
  signum = error "Operator: signum is not meaningful"

addPoly :: Num a => [a] -> [a] -> [a]
addPoly (x : xs) (y : ys) = x + y : addPoly xs ys
addPoly xs [] = xs
addPoly [] ys = ys

instance Scalar a => Show (Operator a) where
  show (Operator cs) =
    renderSum "" [(c, mono k) | (k, c) <- reverse (zip [0 :: Int ..] cs), not (isZero c)]
    where
      mono 0 = ""
      mono 1 = "E"
      mono k = "E^" ++ show k

-- | The degree of an operator, as a polynomial in @E@.
degree :: Operator a -> Int
degree (Operator cs) = length cs - 1

-- | Apply an operator to a sequence @f(0), f(1), …@.
--
-- >>> take 5 (apply (shift - 2) (iterate (* 2) 1) :: [Rational])
-- [0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
-- >>> take 5 (apply (shift * shift - 1) (iterate (* 2) 1) :: [Rational])
-- [3 % 1,6 % 1,12 % 1,24 % 1,48 % 1]
apply :: Num a => Operator a -> [a] -> [a]
apply (Operator cs) f =
  foldr (zipWith (+)) (map (const 0) f) [map (c *) (drop k f) | (k, c) <- zip [0 ..] cs]

-- | Does a nonzero operator send the sequence to zero (checked on a prefix)?
annihilates :: Scalar a => Operator a -> [a] -> Bool
annihilates op f = op /= 0 && all isZero (take 64 (apply op f))

-- | Evaluate an operator as a polynomial at a point (Horner's rule).
evalAt :: Num a => a -> Operator a -> a
evalAt x (Operator cs) = foldr (\c acc -> c + x * acc) 0 cs

-- | Divide out a factor @(E - r)@ by synthetic division, discarding the
-- remainder.
--
-- >>> deflate 2 (shift * shift - 3 * shift + 2 :: Operator Rational)
-- E - 1
deflate :: Num a => a -> Operator a -> Operator a
deflate r (Operator cs) = Operator (drop 1 (reverse (scanl1 (\acc c -> c + r * acc) (reverse cs))))
