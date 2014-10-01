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
