{- |
Module      :  Euler.P065
Description :  Problem 65
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

http://projecteuler.net/problem=65
-}
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

