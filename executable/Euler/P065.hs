module Euler.P065
       ( solve
       ) where

import Data.Char
import           Data.Ratio
import           Euler.Numbers (continuedFractionExpansion)

solve :: Monad m => m Int
solve = return . sum . map digitToInt . show . numerator
    $ continuedFractionExpansion (eContinuedFractions, []) 100

eContinuedFractions :: [Int]
eContinuedFractions = 2:go 1
    where
      go k = 1:2*k:1:go (k+1)

