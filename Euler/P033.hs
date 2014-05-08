{- |
Module      :  Euler.P033
Description :  Problem 033
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Digit canceling fractions

The fraction 49/98 is a curious fraction, as an inexperienced
mathematician in attempting to simplify it may incorrectly believe
that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction,
less than one in value, and containing two digits in the numerator and
denominator.

If the product of these four fractions is given in its lowest common
terms, find the value of the denominator.


-}
module Euler.P033
       ( solve
       ) where

import Data.Ratio

solve :: IO Int
solve = (return . denominator . product)
        [ i%k
        | i <- [1..9]
        , j <- [1..9], k <- [i+1..9]
        , isDigitCancellingFraction i j k ]

isDigitCancellingFraction :: Int -> Int -> Int -> Bool
isDigitCancellingFraction i j k = i /= k && (i*10 + j) % (j*10 + k) == i % k
