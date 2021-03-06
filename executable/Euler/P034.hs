module Euler.P034
       ( solve
       ) where

import           Data.List     (sortBy)
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as V
import           Euler.Numbers (toDigits)

fd :: Vector Int
fd = V.fromList $ fmap (\n -> product [1..n]) [0..9]

solve :: Monad m => m Int
solve = let cs = takeWhile (not . null) $ fmap digitFactorialCandidates [2..]
            fds = filter isFactorialDigitSum $ concat cs
        in return . sum $ fmap (sum . fmap fac) fds

fac :: Int -> Int
fac n = fd ! n

isFactorialDigitSum :: [Int] -> Bool
isFactorialDigitSum xs = xs == (sortBy (flip compare) . toDigits . sum . fmap fac) xs

digitFactorialCandidates :: Int -> [[Int]]
digitFactorialCandidates n = [ x:xs
                             | x <- [9,8..1]
                             , n * fac x >= lb
                             , fac x < ub
                             , xs <- candidates' x (fac x) (n-1)
                             ]
  where
    ub = 10^n
    lb = 10^(n-1)

    candidates' u acc i
      | i == 0 = [[]]
      | null validNumbers = [[]]
      | acc + i*fac u < lb = [[]]
      | otherwise = [ x:xs | x <- validNumbers, xs <- candidates' x (acc + fac x) (i-1) ]
      where
        validNumbers = takeWhile (\x -> acc + fac x < ub) [0..u]
