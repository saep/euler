{- |
Module      :  Euler.P052
Description :  Problem 52
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Permuted multiples

It can be seen that the number, 125874, and its double, 251748, contain exactly
the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain
the same digits.
-}
module Euler.P052
       ( solve
       ) where

import Data.List ((\\))

solve :: IO ()
solve = print $ head [ x | x <- [10..]
                     , let sx = show x
                     , head sx == '1'
                     , head (tail sx) < '7'
                     , all (null . (\\ sx)) (fmap (show . (*x)) [2..6]) ]

