{- |
Module      :  Euler.P018
Description :  Problem 018
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Maximum path sum I

By starting at the top of the triangle below and moving to adjacent
numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

<- text/P018.txt ->

NOTE: As there are only 16384 routes, it is possible to solve this
problem by trying every route. However, Problem 67, is the same
challenge with a triangle containing one-hundred rows; it cannot be
solved by brute force, and requires a clever method! ;o)

-}
module Euler.P018
       ( solve
       , maximumPathSum
       ) where

import           Control.Applicative

solve :: IO Int
solve = fromInteger <$> maximumPathSum "text/P018.txt"

maximumPathSum :: FilePath -> IO Integer
maximumPathSum file = do
  rows <- fmap (fmap read . words) . lines <$> readFile file
  return $ maximumPathSum' rows

maximumPathSum' :: [[Integer]] -> Integer
maximumPathSum' = maximum . foldl (\m -> f (0:m++[0])) []
  where
    f :: [Integer] -> [Integer] -> [Integer]
    f ys@(x:y:_) (a:as) = maximum [a+x,a+y] : f (tail ys) as
    f _ _ = []
