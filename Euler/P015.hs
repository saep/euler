{- |
Module      :  Euler.P015
Description :  Problem 015
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to
move to the right and down, there are exactly 6 routes to the bottom
right corner.

How many such routes are there through a 20×20 grid?

-}
module Euler.P015
       ( solve
       ) where

-- | (a+b)! /(a! * b!)
solve :: IO Int
solve = return . fromInteger $ (fac (20+20) `div` (fac 20 * fac 20))

fac :: Integer -> Integer
fac n = product [1..n]
