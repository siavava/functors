module Main (main) where

import Control.Monad (unless)
import Data.Complex (realPart)
import Data.List (permutations)
import MyLib.Algorithms.Recurrences
import System.Exit (exitFailure)

check :: String -> Bool -> IO Bool
check name ok = do
  unless ok (putStrLn ("FAIL: " ++ name))
  pure ok

main :: IO ()
main = do
  results <-
    sequence
      [ check "three recurrences describe the identity" $
          all (\f -> agreeOn f id [0 .. 200]) [identityA, identityB, identityC]
      , check "hanoi = 2^n - 1" $ agreeOn hanoi hanoiClosed [0 .. 40]
      , check "hanoi unrolled k times" $ and [hanoiUnrolled k 20 == hanoi 20 | k <- [0 .. 20]]
      , check "hanoi moves" $ and [toInteger (length (hanoiMoves n)) == hanoi n | n <- [0 .. 12]]
      , check "phi^(n-2) <= F(n) <= phi^(n-1)" $
          and [lo <= f + 1e-9 && f <= hi + 1e-9 | n <- [1 .. 60], let (lo, hi) = fibonacciBounds n, let f = fromInteger (fibonacci n)]
      , check "mergesort worst case closed form" $ agreeOn mergeSortWorstCase mergeSortWorstCaseClosed [1 .. 2000]
      , check "mergesort worst case is attained" $
          and [maximum (map (toInteger . snd . mergeSortCounting) (permutations [1 .. n :: Int])) == mergeSortWorstCase n | n <- [1 .. 7]]
      , check "mergesort tree = n lg n + n" $
          and [solveTree mergesort (2 ^ k) == fromIntegral (toInteger k * 2 ^ k + 2 ^ k) | k <- [0 .. 12 :: Int]]
      , check "tree level sums" $
          series (recursionTree mergesort 4096) == Level
            && series (recursionTree randomizedSelection 4096) == Decreasing
            && series (recursionTree karatsuba 4096) == Increasing
            && series (recursionTree rootSplit 65536) == Level
      , check "quicksort leaves between log4 n and log4/3 n" $
          let (lo, hi) = leafDepths (recursionTree randomizedQuicksort 4096)
           in lo == 6 && hi == 29
      , check "master theorem" $
          show (master 2 2 1 0) == "Θ(n log n)"
            && show (master 3 2 1 0) == "Θ(n^1.585)"
            && show (master 1 (4 / 3) 1 0) == "Θ(n)"
            && show (master 4 2 1 1) == "Θ(n^2)"
      , check "akra-bazzi" $
          show (akraBazzi [(1, 4 / 3), (1, 4)] 1 0) == "Θ(n log n)"
            && show (akraBazzi [(1, 5), (1, 10 / 7)] 1 0) == "Θ(n)"
            && show (akraBazzi [(1 / 4, 4), (3 / 4, 4 / 3)] 0 0) == "Θ(log n)"
            && near (akraBazziExponent [(1, 2), (1, 4)]) (logBase 2 golden)
      , check "operators factor like polynomials" $
          (shift - 1) * (shift - 2) == (shift * shift - 3 * shift + 2 :: Operator Rational)
      , check "(E - a)^d annihilates p(n) a^n" $
          annihilates ((shift - 3) ^ (3 :: Int)) ([fromIntegral (n * n + 1) * 3 ^ n | n <- [0 :: Int ..]] :: [Rational])
      , check "annihilator kills its recurrence" $
          all (\l -> annihilates (annihilator l) (values l)) linears
      , check "exact solutions match the recurrences" $
          and [take 30 (values l) == map (evaluate c) [0 .. 29] | l <- linears, Just c <- [solveExact l]]
      , check "numeric solutions match the recurrences" $
          and
            [ and [abs (realPart (evaluate c n) - fromRational v) < 1e-6 * max 1 (abs (fromRational v)) | (n, v) <- zip [0 .. 29] (values l)]
            | l <- linears
            , Just c <- [solve l]
            ]
      , check "binet" $
          maybe False (\c -> and [round (realPart (evaluate c n)) == fibonacci n | n <- [0 .. 70]]) (solve fibonacciRecurrence)
      , check "cubic example" $ fmap show (solveExact cubicRecurrence) == Just "n·2^n - 3·2^n + 4"
      , check "mixed example is Θ(n 2^n)" $ fmap (show . asymptotic) (solveExact mixedRecurrence) == Just "Θ(n 2^n)"
      , check "ham-sandwich domain transformation" $
          map (fromInteger . hamSandwichExact . (2 ^)) [0 .. 25 :: Int] == take 26 (values hamSandwichDomain)
            && fmap (show . substituteLog . asymptotic) (solve hamSandwichDomain) == Just "Θ(n^0.6942)"
      , check "mergesort domain transformation" $
          fmap (show . substituteLog . asymptotic) (solveExact mergesortDomain) == Just "Θ(n log n)"
      , check "difference inverts partial sums" $
          let xs = take 20 (values fibonacciRecurrence) in difference (partialSums xs) == drop 1 xs
      , check "quicksort full history = limited history = closed form" $
          take 60 quicksortFullHistory == take 60 quicksortLimitedHistory
            && take 60 quicksortFullHistory == map quicksortClosed [0 .. 59]
      , check "average quicksort comparisons" $
          and [averageComparisons n == expectedComparisons !! n | n <- [0 .. 7]]
            && take 60 expectedComparisons == map expectedComparisonsClosed [0 .. 59]
      ]
  let failures = length (filter not results)
  if failures == 0
    then putStrLn ("All " ++ show (length results) ++ " checks passed.")
    else putStrLn (show failures ++ " checks failed.") >> exitFailure
  where
    linears =
      [ fiveFoldRecurrence
      , fibonacciRecurrence
      , hanoiRecurrence
      , heightBalancedRecurrence
      , cubicRecurrence
      , mixedRecurrence
      , hamSandwichDomain
      , mergesortDomain
      ]
