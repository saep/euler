{- |
Module      :  Euler.P026
Description :  Problem 026
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Reciprocal cycles
Problem 26

A unit fraction contains 1 in the numerator. The decimal
representation of the unit fractions with denominators 2 to 10 are
given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest
recurring cycle in its decimal fraction part.

-}
module Euler.P026
       ( solve
       ) where

import qualified Data.IntMap as M
import           Data.List   (maximumBy)

solve :: IO Int
solve = return . snd . maximumBy (\(x,_) (y,_) -> compare x y) $
        fmap (\x -> (reciprocalCycleLength x, x)) [999,998..1]

-- | The function d calculates the first number, for which @x@ is
-- greater than @n@. @n@ is the fraction for which we search the
-- length of the reciprocal cycle. With this helper function we
-- successively divide the remainder of the accumulating number by @n@
-- until we hit a remainder that we had before. The cycle length is
-- then the current index minus the index stored for that remainder.
reciprocalCycleLength :: Int -> Int
reciprocalCycleLength n = go M.empty 1 (d 1 `mod` n)
  where
    d x = head . dropWhile (<n) $ fmap (*x) [1,10..]

    go :: M.IntMap Int -> Int -> Int -> Int
    go rems i r
      | r == 0 = 0
      | otherwise = case M.lookup r rems of
        Just j -> i-j
        Nothing -> go (M.insert r i rems) (i+1) (d r `mod` n)
