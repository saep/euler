{- |
Module      :  Euler.P003
Description :  Solution for riddle 3.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

-}
module Euler.P003
       ( solve
       ) where

import Euler.Prime

solve :: IO ()
solve = print . maximum $ factorizeSingleNumber 600851475143
