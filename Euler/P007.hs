{- |
Module      :  Euler.P007
Description :  Problem 7
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
see that the 6th prime is 13.

What is the 10 001st prime number?

-}
module Euler.P007
       ( solve
       ) where

import Euler.Prime (primes)

solve :: IO Int
solve = return $ primes !! 10000 -- Index starting at 0!
