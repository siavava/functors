-- | Solving recurrences, after Jeff Erickson's /Algorithms/, Appendix II.
--
-- Methods:
--
-- * "MyLib.Algorithms.Recurrences.Guess": run the recurrence, guess, confirm.
-- * "MyLib.Algorithms.Recurrences.Trees": recursion trees.
-- * "MyLib.Algorithms.Recurrences.AkraBazzi": the Master and Akra–Bazzi
--   theorems.
-- * "MyLib.Algorithms.Recurrences.Annihilators": operators and annihilators
--   for linear recurrences.
-- * "MyLib.Algorithms.Recurrences.Transformations": domain, range, and
--   difference transformations.
-- * "MyLib.Algorithms.Recurrences.Asymptotic": the @Θ@ classes they all
--   produce.
--
-- Worked examples, by algorithm, are in
-- "MyLib.Algorithms.Recurrences.Examples".
module MyLib.Algorithms.Recurrences
  ( module MyLib.Algorithms.Recurrences.Asymptotic
  , module MyLib.Algorithms.Recurrences.Guess
  , module MyLib.Algorithms.Recurrences.Trees
  , module MyLib.Algorithms.Recurrences.AkraBazzi
  , module MyLib.Algorithms.Recurrences.Annihilators
  , module MyLib.Algorithms.Recurrences.Transformations
  , module MyLib.Algorithms.Recurrences.Examples
  ) where

import MyLib.Algorithms.Recurrences.AkraBazzi
import MyLib.Algorithms.Recurrences.Annihilators
import MyLib.Algorithms.Recurrences.Asymptotic
import MyLib.Algorithms.Recurrences.Examples
import MyLib.Algorithms.Recurrences.Guess
import MyLib.Algorithms.Recurrences.Transformations
import MyLib.Algorithms.Recurrences.Trees
