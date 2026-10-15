-- | Guess and confirm: the one fail-safe method for solving any recurrence.
--
-- A recurrence with only equations has a unique solution, namely the
-- function computed by turning it into a recursive algorithm. So we can
-- always /run/ a recurrence, eyeball its values, guess a closed form, and
-- check the guess against the recurrence itself.
module MyLib.Algorithms.Recurrences.Guess
  ( memoFix
  , agreeOn
  , ratios
  ) where

-- | Evaluate a recurrence on the non-negative integers, memoising every
-- value so each is computed once. The recurrence receives a handle to its
-- own (memoised) solution in place of explicit recursion.
--
-- >>> memoFix (\f n -> if n < 2 then toInteger n else f (n - 1) + f (n - 2)) 90
-- 2880067194370816120
memoFix :: ((Int -> a) -> Int -> a) -> Int -> a
memoFix f = (table !!)
  where
    table = map (f (table !!)) [0 ..]

-- | Do two functions agree on every given input? This is the finite,
-- executable shadow of an induction proof.
agreeOn :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
agreeOn f g = all (\x -> f x == g x)

-- | Ratios @T(n) / g(n)@ along a sequence of inputs. If they settle near a
-- positive constant, @T(n) = Θ(g(n))@ is a good guess; if they drift to zero
-- or infinity, the guess is too big or too small.
ratios :: (Double -> Double) -> (Double -> Double) -> [Double] -> [Double]
ratios t g = map (\n -> t n / g n)
