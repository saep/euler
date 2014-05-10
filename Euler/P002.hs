{- |
Module      :  Euler.P002
Description :  Solution for riddle 2.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

| Even Fibonacci numbers

Each new term in the Fibonacci sequence is generated by adding the
previous two terms. By starting with 1 and 2, the first 10 terms
will be:

> 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do
not exceed four million, find the sum of the even-valued terms.

-}
module Euler.P002
       ( solve
       ) where

import           Euler.Formulae

solve :: IO Int
solve = return . fromInteger . sum . filter even $ takeWhile (< 4000000) fibs
