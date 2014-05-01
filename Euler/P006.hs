{- |
Module      :  Euler.P006
Description :  Problem 6
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Sum square difference

The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten
natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.

-}
module Euler.P006
       ( solve
       ) where

import Euler.Formulae (sumTo, squareOfSum)

solve :: IO ()
solve = print . abs $ squareOfSum 100 - sumTo 100^2
