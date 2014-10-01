module Euler.P008
       ( solve
       ) where

import           Control.Applicative
import           Data.Char

solve :: IO Int
solve = filter isDigit <$> readFile "text/P008.txt" >>=
        return . biggestProduct 0 . fmap digitToInt

biggestProduct :: Int -> [Int] -> Int
biggestProduct mp [] = mp
biggestProduct mp ns = biggestProduct ((max mp . product . take 5) ns) (tail ns)
