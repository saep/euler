{- |
Module      :  Euler.P005
Description :  Solution for riddle 5.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Smallest multiple

2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all
of the numbers from 1 to 20?

-}
module Euler.P005
       ( solve
       ) where

import Euler.Formulae (gcf)

solve :: IO ()
solve = print $ foldl1 (\a b -> a * b `div` gcf a b) [1..20]
