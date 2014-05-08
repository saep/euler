{- |
Module      :  Euler.P011
Description :  Problem 11
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

In the $20 \times 20$ grid below, four numbers along a diagonal line
have been marked in red.

<- stored in text/P011.txt ->

The product of these numbers is $26 × 63 × 78 × 14 = 1788696$.

What is the greatest product of four adjacent numbers in the same
direction (up, down, left, right, or diagonally) in the $20 \times
20$ grid?

-}
module Euler.P011
       ( solve
       ) where

import Control.Applicative
import Data.List

solve :: IO Int
solve = return . maxProduct =<<
        fmap (fmap read . words) . lines <$> readFile "text/P011.txt"

maxProduct :: [[Int]] -> Int
maxProduct m = maximum $ concatMap ($ m) [ diagonalProducts
                                     , diagonalProducts . (reverse <$>)
                                     , horizontalProducts
                                     , verticalProducts
                                     ]

diagonalProducts :: [[Int]] -> [Int]
diagonalProducts m@(r1:(_:r2):(_:_:r3):(_:_:_:r4):_) =
  verticalProducts [r1,r2,r3,r4] ++ diagonalProducts (tail m)
diagonalProducts _ = []

verticalProducts :: [[Int]] -> [Int]
verticalProducts m@(a:b:c:d:_) = zipWith4 (\i j k l -> i*j*k*l) a b c d
                                 ++ verticalProducts (tail m)
verticalProducts _ = []

horizontalProducts :: [[Int]] -> [Int]
horizontalProducts = concatMap lineProducts
  where
    lineProducts :: [Int] -> [Int]
    lineProducts l@(_:_:_:_:_) = product (take 4 l) : lineProducts (tail l)
    lineProducts _ = []
