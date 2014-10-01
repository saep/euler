module Euler.P048
       ( solve
       ) where

solve :: Monad m => m Int
solve = return . fromInteger . (`mod` 10000000000) . sum
               . fmap truncateSelfpower $ [1..1000]

truncateSelfpower :: Integer -> Integer
truncateSelfpower n
    | n `mod` 10 == 0 = 0
    | otherwise = n^n `mod` 10000000000

