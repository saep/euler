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
