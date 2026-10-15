-- | Asymptotic growth classes: the answers every technique in
-- "MyLib.Algorithms.Recurrences" ultimately resolves a recurrence to.
module MyLib.Algorithms.Recurrences.Asymptotic
  ( Theta (..)
  , constant
  , substituteLog
  , near
  , showNum
  ) where

import Numeric (showFFloat)

-- | The growth class @Θ(n^p · (log n)^k · (log log n)^j · r^n)@.
data Theta = Theta
  { expBase   :: Double
  , polyExp   :: Double
  , logExp    :: Double
  , loglogExp :: Double
  }
  deriving (Eq)

-- | Exponentials dominate polynomials, which dominate logarithms, which
-- dominate iterated logarithms, so growth classes compare lexicographically.
instance Ord Theta where
  compare (Theta r p k j) (Theta r' p' k' j') = compare (r, p, k, j) (r', p', k', j')

-- |
-- >>> Theta 1 1 1 0
-- Θ(n log n)
-- >>> Theta 2 1 0 0
-- Θ(n 2^n)
-- >>> Theta 1 (logBase 2 3) 0 0
-- Θ(n^1.585)
instance Show Theta where
  show (Theta r p k j) = "Θ(" ++ body ++ ")"
    where
      body = case filter (not . null) [poly, logs "log" k, logs "log log" j, expo] of
        [] -> "1"
        fs -> unwords fs
      poly
        | near p 0 = ""
        | near p 1 = "n"
        | otherwise = "n^" ++ showNum p
      logs name x
        | near x 0 = ""
        | near x 1 = name ++ " n"
        | otherwise = name ++ "^" ++ showNum x ++ " n"
      expo
        | near r 1 = ""
        | otherwise = showNum r ++ "^n"

-- | @Θ(1)@.
constant :: Theta
constant = Theta 1 0 0 0

-- | Rewrite a growth class in @k@ as one in @n@, where @k = lg n@.
--
-- This undoes the domain transformation @t(k) = T(2^k)@: an exponential
-- @r^k@ becomes the polynomial @n^(lg r)@, and every polynomial in @k@
-- becomes a polylogarithm in @n@. Iterated logarithms of @k@ have no
-- counterpart here, so @loglogExp@ must be zero.
--
-- >>> substituteLog (Theta 2 1 0 0)
-- Θ(n log n)
substituteLog :: Theta -> Theta
substituteLog (Theta r p k _) = Theta 1 (logBase 2 r) p k

-- | Approximate equality, for comparing exponents computed in floating point.
near :: Double -> Double -> Bool
near x y = abs (x - y) < 1e-9

-- | Show a number without trailing noise: integers as integers, everything
-- else to four decimal places.
--
-- >>> showNum 2.000000001
-- "2"
-- >>> showNum ((1 + sqrt 5) / 2)
-- "1.618"
showNum :: Double -> String
showNum x
  | near x (fromIntegral n) = show n
  | otherwise = trim (showFFloat (Just 4) x "")
  where
    n = round x :: Integer
    trim = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse
