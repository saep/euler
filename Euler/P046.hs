{- |
Module      :  Euler.P046
Description :  Problem 46

Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Goldbach's other conjecture
Problem 46

It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square?

-}
module Euler.P046
       ( solve
       ) where

import           Euler.Prime

solve :: IO Int
solve = return $ until (not . testProperty) (+2) 9

testProperty :: Int -> Bool
testProperty c =
    let dsqs = reverse . takeWhile (<c) $ fmap (\n -> 2*n*n) [1..]
    in isPrime c || any (isPrime . (c-)) dsqs
