module Euler.P024
       ( solve
       ) where

import           Data.Char

solve :: Monad m => m Int
solve = return . read . fmap (chr . (+) (ord '0'))
               $ findNthPermutation 999999 [0..9]

fac :: Integral i => i -> Integer
fac n = product [1..fromIntegral n]

findNthPermutation :: Integer -> [a] -> [a]
findNthPermutation n xxs@(x:xs)
  | n == 0 = xxs
  | f > n = x:findNthPermutation n xs
  | otherwise = let i = n `div` f
                    n' = n - i*f
                    (as,b:bs) = splitAt (fromIntegral i) xxs
                in b : findNthPermutation n' (as++bs)
  where
    f = fac $ length xs

findNthPermutation _ _ = []
