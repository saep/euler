{- |
Module      :  Euler.P013
Description :  Problem 13
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Large sum
Problem 13

Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
-}
module Euler.P013
       ( solve
       ) where

import Control.Applicative

-- | The maximum amount of carry that can be created is (when starting
-- from the right) 900. If the second column of digits is all nines as
-- well, we get a carry of 990 for the next column and then we cann
-- add 90 (900/10) again to get the next maximum carry of 1080. If you
-- continue this, you see that we get at most 3 more digits out of
-- it. and that 3 digits later the carry is summed in and has no real
-- impact anymore. As the number do not start with a leading zero, the
-- minimum carry for the left-most column is 100 which adds two
-- columns to the sum. That means that the resulting number can vary
-- in one column. So, as the reach for the carry is only two digits
-- and we have to add at least two digits, we only need to consider
-- 10+3-2=11 digits of the numbers.
solve :: IO Int
solve = read . take 10 . show . sum . fmap (read . take 11) . lines
        <$> readFile "text/P013.txt"
