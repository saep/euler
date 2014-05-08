{- |
Module      :  Euler.P016
Description :  Problem 16
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Power digit sum

215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

-}
module Euler.P016
       ( solve
       ) where

import Data.Char

solve :: IO Int
solve = return . sum . fmap digitToInt . show $ 2^1000
