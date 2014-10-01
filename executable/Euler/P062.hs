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

