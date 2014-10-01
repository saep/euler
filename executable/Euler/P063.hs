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

