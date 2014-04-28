{- |
Module      :  Euler.P043
Description :  Problem 043
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Sub-string divisibility

The number, 1406357289, is a 0 to 9 pandigital number because it is
made up of each of the digits 0 to 9 in some order, but it also has a
rather interesting sub-string divisibility property.

Let d(1) be the 1st digit, d(2) be the 2nd digit, and so on. In this
way, we note the following:

    d(2) d(3) d(4)  = 406 is divisible by 2
    d(3) d(4) d(5)  = 063 is divisible by 3
    d(4) d(5) d(6)  = 635 is divisible by 5
    d(5) d(6) d(7)  = 357 is divisible by 7
    d(6) d(7) d(8)  = 572 is divisible by 11
    d(7) d(8) d(9)  = 728 is divisible by 13
    d(8) d(9) d(10) = 289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.

-}
module Euler.P043
       ( solve
       ) where

import Data.Char
import Data.List ((\\), delete, foldl')

type RemainingCharSet = String

startValue :: [(String, RemainingCharSet)]
startValue = [ (x', set) | x <- [17,34..999] :: [Int]
             , let x' = if x < 100 then '0':show x else show x
             , let set = ['0'..'9'] \\ x'
             , length set == 7 ]

generate :: [Int]
generate = fmap (read . fst) . foldl' next startValue $ [13,11,7,5,3,2,1]

next :: [(String, RemainingCharSet)] -> Int -> [(String, RemainingCharSet)]
next cs d = [ (y:x', y `delete` s) | (x', s) <- cs
            , let x = read (take 2 x')
            , y <- s
            , (digitToInt y * 100 + x) `mod` d == 0
            ]

solve :: IO ()
solve = print $ sum generate
