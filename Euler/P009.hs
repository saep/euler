{- |
Module      :  Euler.P009
Description :  Problem 9
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which, a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c =
1000.  Find the product abc.

-}
module Euler.P009
       ( solve
       ) where

import Euler.Numbers (pythagoreanTriplets)

solve :: IO Int
solve = return . (\(a,b,c) -> a*b*c) . head
        . filter (\(a,b,c) -> a+b+c == 1000)
        $ fmap (head . dropWhile (\(a,b,c) -> a+b+c < 1000)) pythagoreanTriplets
