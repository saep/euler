module Euler.P011
       ( solve
       ) where

import           Control.Applicative
import           Data.List

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
