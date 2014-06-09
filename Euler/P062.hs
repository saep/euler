{- |
Module      :  Euler.P062
Description :  Problem 62
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Cubic permutations

The cube, 41063625 (345³), can be permuted to produce two other cubes: 56623104
(384³) and 66430125 (405³). In fact, 41063625 is the smallest cube which has
exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are
cube.

-}
module Euler.P062
       ( solve
       ) where

import           Control.Applicative
import           Data.Function       (on)
import           Data.List           (groupBy, sort, sortBy)
import           Euler.Numbers       (toDigits)

solve :: Monad m => m Int
solve = return $ findCubicNPermutation 5

cube :: Int -> (Int, Int)
cube n = (n, n*n*n)

findCubicNPermutation :: Int -> Int
findCubicNPermutation n =
    -- extract the smallest cube
    snd . cube . head . fmap fst
    -- find the smallest n-permutation
    . head . filter ((==n) . length)
    -- sort the digits of the cube and group equal ones
    . concatMap (groupBy ((==) `on` snd) . sortBy (compare `on` snd))
    -- chunk by length of the cube
    . groupBy ((==) `on` (length . snd))
    $ (fmap (sort . toDigits) . cube) <$> [345..]

