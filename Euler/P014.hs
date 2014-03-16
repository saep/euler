{- |
Module      :  Euler.P014
Description :  Problem 14
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Longest Collatz sequence

The following iterative sequence is defined for the set of positive
integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one
million.

-}
module Euler.P014
       ( solve
       ) where

import qualified Data.IntMap as M
import Data.Maybe

solve :: IO ()
solve = print $ findLongestCollatz [500001,500003..999999] (1,1) (M.singleton 1 1)

findLongestCollatz :: [Int] -> (Int, Int) -> M.IntMap Int -> Int
findLongestCollatz (x:xs) curMax m
  | x `M.member` m = findLongestCollatz xs curMax m
  | otherwise = let (longest, len, m') = updateMap (collatz x) curMax m
                in findLongestCollatz xs (longest, len) m'
findLongestCollatz _ (longest, _) _ = longest

updateMap :: [Int] -> (Int, Int) -> M.IntMap Int -> (Int, Int, M.IntMap Int)
updateMap cs (longest, len) m =
  let (xs, l:_) = span (\k -> isNothing (M.lookup k m)) cs
      len' = length xs + (fromJust $ M.lookup l m)
      toInsert = zip xs [len',(len'-1)..]
      m' = foldr (\(n,ln) -> M.insert n ln) m toInsert
  in case compare len len' of
    LT -> (head xs, len', m')
    _ -> (longest, len, m')

collatz :: Int -> [Int]
collatz n
  | n == 1 = [1]
  | odd n = n : collatz (3*n + 1)
  | otherwise = n : collatz (n `div` 2)
