{- |
Module      :  Euler.P063
Description :  Problem 63
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Powerful digit counts

The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
-}
module Euler.P063
       ( solve
       ) where

{-
    length x^n = n
<=> 1 + (floor . (n*) . logBase 10) x = n
<=> 1 + n * logBase 10 x < n + 1  due to integer constraints
<=> n * logBase 10 x < n
<=> logBase 10 x < 1
<=> x < 10
-}

solve :: Monad m => m Int
solve = return $ length
    [ () | x <- [1..10]
    , _ <- takeWhile (\(n,xn) -> n == (length . show) xn)
        (zip [1..] (iterate (*x) x))
    ]

