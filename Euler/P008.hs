{- |
Module      :  Euler.P008
Description :  Problem 8
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Largest product in a series
Problem 8

Find the greatest product of five consecutive digits in the 1000-digit number.

<- stored in text/P008.txt ->

-}
module Euler.P008
       ( solve
       ) where

import Control.Applicative
import Data.Char

solve :: IO ()
solve = filter isDigit <$> readFile "text/P008.txt" >>=
        print . biggestProduct 0 . fmap digitToInt

biggestProduct :: Int -> [Int] -> Int
biggestProduct mp [] = mp
biggestProduct mp ns = biggestProduct ((max mp . product . take 5) ns) (tail ns)
