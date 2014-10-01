module Euler.P035
       ( solve
       ) where

import           Data.List     (foldl', sort)
import           Euler.Numbers
import           Euler.Prime   (isPrime)
import           Euler.SList   (snub)

solve :: Monad m => m Int
solve = return (circularPrimes 6)

circles :: [Int] -> [Int]
circles ps = snub . sort $ foldl' f [num] [0..l-2]
  where
    l = length ps
    p = 10^(l-1)
    num = toNum ps

    f cs@(c:_) _ = let (d,r) = c `divMod` 10 in r*p + d : cs


circularPrimes :: Int -> Int
circularPrimes n = foldl' f 4 [2..n]
  where
    f acc = (+) acc . sum . fmap length . filter (all isPrime) . candidates

candidates :: Int -> [[Int]]
candidates n = (snub . sort) [ circles (x:xs)
                             | x <- [1,3,7]
                             , xs <- pick (dropWhile (<x) [1,3,7,9]) (n-1)
                             ]
  where
    pick _ 0 = [[]]
    pick ps c = [ x:xs | x <- ps, xs <- pick ps (c-1) ]
