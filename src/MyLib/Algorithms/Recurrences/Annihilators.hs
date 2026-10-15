-- | Linear recurrences, solved by annihilators. The method is five steps:
--
-- 1. write the recurrence in operator form ('characteristic'),
-- 2. extract an annihilator ('annihilator'),
-- 3. factor it ('rationalRoots', 'roots'),
-- 4. read off the generic solution: each factor @(E - r)^m@ contributes
--    @n^d r^n@ for @d < m@,
-- 5. fit the coefficients to the first few values ('solveExact', 'solve').
module MyLib.Algorithms.Recurrences.Annihilators
  ( -- * Scalars
    Scalar (..)
    -- * Operators
  , Operator (..)
  , operator
  , shift
  , degree
  , apply
  , annihilates
  , evalAt
    -- * Closed forms
  , Term (..)
  , ClosedForm (..)
  , evaluate
  , annihilatorOf
  , asymptotic
    -- * Linear recurrences
  , Linear (..)
  , homogeneous
  , values
  , characteristic
  , annihilator
    -- * Factoring
  , rationalRoots
  , roots
    -- * Solving
  , solveExact
  , solve
  ) where

import MyLib.Algorithms.Recurrences.Annihilators.ClosedForm
import MyLib.Algorithms.Recurrences.Annihilators.Linear
import MyLib.Algorithms.Recurrences.Annihilators.Operator
import MyLib.Algorithms.Recurrences.Annihilators.Roots
import MyLib.Algorithms.Recurrences.Annihilators.Scalar
import MyLib.Algorithms.Recurrences.Annihilators.Solve
