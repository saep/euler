{- |
Module      :  Euler.P001
Description :  Solution for riddle 1.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3
or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

-}

module Euler.P001
       ( solve
       ) where

import           Euler.Formulae (sumTo)

solve :: Monad m => m Int
solve = return $ f 3 + f 5 - f (3*5)
  where
    limit = 999 :: Int
    f n = n * sumTo (limit `div` n)
