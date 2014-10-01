module Euler.P038
       ( solve
       ) where

import           Data.List (delete)

solve :: Monad m => m Int
solve = return maxPandigital

-- | The first number must be a 9, because the example on the webpage
-- starts with a 9 and we search for the largest such number which
-- must be greater or equal to the example.
--
-- 1-Digit: 9 -> see example -> the 1-digit candidate
--
-- 2-Digits: 9x -> would either make a <=8 or >=11 digit number -> no
-- candidates
--
-- 3-Digits: 9xx -> would either make a <=7 or >=11 digit number -> no
-- candidates
--
-- 4-Digits: 9zxx -> would not yield a valid number for z <- [5..9] as
-- 2*9zxx would start with 19 (but we have a 9 already)
--
-- 4-Digits: 91xx -> would not yield a valid number for z = 1 as the 2*9xxx will
-- start with a 1
--
-- 4-Digits: 94xx -> not possible since the third digit of 2*94xx
-- would be 8 or 9 which we both already have.
--
-- 5+-Digits: since we need at least to products, any digit count
-- greater than 4 is invalid
maxPandigital :: Int
maxPandigital = maximum $ 918273645 :
                take 1 [ read c2c
                       | x <- "32"
                       , y <- delete x ['7','6'..'2']
                       , z <- delete x (delete y ['7','6'..'2'])
                       , let c = read ['9',x,y,z] :: Int
                             c2c = show c ++ show (2*c)
                       , all (`elem` c2c) ['1'..'9'] ]
