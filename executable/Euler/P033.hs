module Euler.P033
       ( solve
       ) where

import           Data.Ratio

solve :: Monad m => m Int
solve = (return . denominator . product)
        [ i%k
        | i <- [1..9]
        , j <- [1..9], k <- [i+1..9]
        , isDigitCancellingFraction i j k ]

isDigitCancellingFraction :: Int -> Int -> Int -> Bool
isDigitCancellingFraction i j k = i /= k && (i*10 + j) % (j*10 + k) == i % k
