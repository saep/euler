{- |
Module      :  Euler.P039
Description :  Problem 039
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Integer right triangles

If p is the perimeter of a right angle triangle with integral length
sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

-}
module Euler.P039
       ( solve
       ) where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Euler.Numbers (pythagoreanTriplets)

solve :: IO Int
solve = return . snd . maximum
        . fmap (length &&& head)
        . group . sort
        . concat . takeWhile (not . null)
        . fmap (takeWhile (<= 1000))
        $ fmap (fmap (\(a,b,c) -> a+b+c)) pythagoreanTriplets
