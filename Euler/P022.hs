{- |
Module      :  Euler.P022
Description :  Problem 022
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Names scores

Using names.txt (right click and 'Save Link/Target As...'), a 46K text
file containing over five-thousand first names, begin by sorting it
into alphabetical order. Then working out the alphabetical value for
each name, multiply this value by its alphabetical position in the
list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN,
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

-}
module Euler.P022
       ( solve
       ) where

import Control.Applicative
import Data.Char
import Data.List

solve :: IO ()
solve = print =<< (p22 . read . ('[':) . (++"]") <$> readFile "text/names.txt")

p22 :: [String] -> Integer
p22 = sum . zipWith (*) [1..] . fmap sum .sort
      . fmap (fmap (fromIntegral . subtract (pred (ord 'A')) . ord))
