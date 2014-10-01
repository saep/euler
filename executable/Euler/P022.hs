module Euler.P022
       ( solve
       ) where

import           Control.Applicative
import           Data.Char
import           Data.List

solve :: IO Int
solve = fromInteger . p22 . read . ('[':) . (++"]") <$> readFile "text/names.txt"

p22 :: [String] -> Integer
p22 = sum . zipWith (*) [1..] . fmap sum .sort
      . fmap (fmap (fromIntegral . subtract (pred (ord 'A')) . ord))
