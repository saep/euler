{- |
Module      :  Euler.P057
Description :  Problem 57
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Square root convergents

It is possible to show that the square root of two can be expressed as an
infinite continued fraction.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
    1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth
expansion, 1393/985, is the first example where the number of digits in the
numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator
with more digits than denominator?

-}
module Euler.P057
       ( solve
       ) where

import Data.Ratio

solve :: Monad m => m Int
solve = return . length .
            filter numeratorDigitsExceedDenominatorDigits . take 1000
            $ iterate nextExpansion (3%2)

nextExpansion :: Ratio Integer -> Ratio Integer
nextExpansion e = 1 + 1 / (1 + e)

numeratorDigitsExceedDenominatorDigits :: Ratio Integer -> Bool
numeratorDigitsExceedDenominatorDigits r =
    let n = numerator r
        d = denominator r
    in (length . show) n > (length . show) d
