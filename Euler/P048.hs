{- |
Module      :  Euler.P048
Description :  Problem 48
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Self powers

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-}
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

