module Euler.P061
       ( solve
       ) where

import Control.Applicative
import Data.Function (on)
import Data.List (groupBy, permutations, nub)
import Data.Vector ((!),Vector)
import qualified Data.Vector as Vector
import Euler.Numbers

solve :: Monad m => m Int
solve = return $ sum findCyclicSet

-- Create some lookup arrays which contain lists of numbers for every two-digit
-- number.
mkVector :: [Int] -> Vector [Int]
mkVector = Vector.fromList . construct 0
            . groupBy ((==) `on` fst) . fmap (`divMod` 100)
            . dropWhile (<1000) . takeWhile (<10000)
  where
    construct i (hs@((h,_):_):rs)
        | i < h = [] : construct (i+1) (hs:rs)
        | otherwise = filter (>9) (snd <$> hs) : construct (i+1) rs
    construct i _
        | i > 99 = []
        | otherwise = [] : construct (i+1) []

triangularVector :: Vector [Int]
triangularVector = mkVector triangulars

gonals :: [Vector [Int]]
gonals = mkVector <$> [ scanl1 (+) [1,3..], pentagonals
                      , hexagonals, heptagonals, octagonals ]

findCyclicSet :: [Int]
findCyclicSet = head . filter ((6==) . length)
    . concatMap (fmap (convertToNums triangularVector)
                 . findCyclicSet' [10..99] . (++[triangularVector]))
    $ permutations gonals

convertToNums :: Vector [Int] -> [Int] -> [Int]
convertToNums v xs
    | head xs `elem` (v ! last xs) = nub $ go (last xs : xs)
    | otherwise = []
  where
    go (x:y:ys) = x*100+y : go (y:ys)
    go _ = []

findCyclicSet' :: [Int] -> [Vector [Int]] -> [[Int]]
findCyclicSet' _ [] = [[]]
findCyclicSet' [] _ = []
findCyclicSet' xs (n:ns) = [ x:ys | x <- xs
                           , ys <- findCyclicSet' (n ! x) ns
                           ]

