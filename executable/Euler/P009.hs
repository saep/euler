module Euler.P009
       ( solve
       ) where

import           Euler.Numbers (pythagoreanTriplets)

solve :: Monad m => m Int
solve = return . (\(a,b,c) -> a*b*c) . head
        . filter (\(a,b,c) -> a+b+c == 1000)
        $ fmap (head . dropWhile (\(a,b,c) -> a+b+c < 1000)) pythagoreanTriplets
