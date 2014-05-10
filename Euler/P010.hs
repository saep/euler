{- |
Module      :  Euler.P010
Description :  Problem 10
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Summation of primes

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}
module Euler.P010
       ( solve
       ) where

import           Euler.Prime (primes)

solve :: IO Int
solve = return . sum . fmap (fromIntegral :: Int -> Int)
        $ takeWhile (< 2000000) primes
