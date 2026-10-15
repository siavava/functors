-- | Linear recurrences, and the operators that annihilate them.
module MyLib.Algorithms.Recurrences.Annihilators.Linear
  ( Linear (..)
  , homogeneous
  , values
  , characteristic
  , annihilator
  ) where

import MyLib.Algorithms.Recurrences.Annihilators.ClosedForm (ClosedForm (..), annihilatorOf, evaluate)
import MyLib.Algorithms.Recurrences.Annihilators.Operator (Operator, operator)

-- | The recurrence @T(n) = c₁ T(n-1) + … + c_k T(n-k) + g(n)@ for @n ≥ k@,
-- with the residue @g@ written as a closed form and the initial values
-- @T(0), …, T(k-1)@ given.
data Linear = Linear
  { coefficients :: [Rational]
  , residue      :: ClosedForm Rational
  , initial      :: [Rational]
  }

-- | A recurrence with no residue.
homogeneous :: [Rational] -> [Rational] -> Linear
homogeneous cs = Linear cs (ClosedForm [])

-- | Run the recurrence.
--
-- >>> take 7 (values (homogeneous [1, 1] [0, 1]))
-- [0 % 1,1 % 1,1 % 1,2 % 1,3 % 1,5 % 1,8 % 1]
values :: Linear -> [Rational]
values (Linear cs g xs) = xs ++ go (length xs) (reverse xs)
  where
    go n prev = x : go (n + 1) (take (length cs) (x : prev))
      where
        x = sum (zipWith (*) cs prev) + evaluate g n

-- | Step 1: the recurrence in operator form. Its left side is
-- @(E^k - c₁ E^(k-1) - … - c_k) T@.
characteristic :: Linear -> Operator Rational
characteristic l = operator (reverse (1 : map negate (coefficients l)))

-- | Step 2: annihilate the residue too. Shifting the residue does not
-- change its annihilator, so the product kills the whole recurrence.
--
-- >>> import MyLib.Algorithms.Recurrences.Annihilators.ClosedForm (Term (..))
-- >>> annihilator (Linear [2] (ClosedForm [Term 1 0 1]) [0])
-- E^2 - 3E + 2
annihilator :: Linear -> Operator Rational
annihilator l = annihilatorOf (residue l) * characteristic l
