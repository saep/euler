module Euler.P040
       ( solve
       ) where

import           Data.Char

solve :: Monad m => m Int
solve = return . product $ fmap (d . (10^)) [0..6]

d :: Integer -> Int
d i = d' 0 (i-1) [1..]

d' :: Integer -> Integer -> [Integer] -> Int
d' n i (d:ds)
  | i <= is = digitToInt $ show (n + 1 + i `div` d) !! fromInteger (i`mod`d)
  | otherwise = d' (n + is`div`d) (i-is) ds
  where
    is = d*9*10^(d-1)
