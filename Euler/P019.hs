{- |
Module      :  Euler.P019
Description :  Problem 019
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Counting Sundays
Problem 19

You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?

-}
module Euler.P019
       ( solve
       ) where

type Year = Integer

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
             deriving (Show, Eq, Ord, Enum)

type Day = Integer

type Month = Integer

solve :: Monad m => m Int
solve = return $ length [ () | m <- [1..12], y <- [1901..2000], isWeekDay (y,m,1) Sunday ]

isLeapYear :: Year -> Bool
isLeapYear y = ((y `mod` 4 == 0) && (y `mod` 100 /= 0)) || (y `mod` 400 == 0)

isWeekDay :: (Year, Month, Day) -> WeekDay -> Bool
isWeekDay (y, m, d) w = weekDay (y,m,d)  == w

weekDay :: (Year, Month, Day) -> WeekDay
weekDay (y,m,d) = toEnum . fromIntegral $ daysAfter19000101 (y,m,d) `mod` 7

daysAfter19000101 :: (Year, Month, Day) -> Integer
daysAfter19000101 (y,m,d)
  | y < 1900 = error "Dates before 1900 are not (yet) supported."
  | otherwise = 365 * (y - 1900) + leapYears (1900,y-1)
                + sum (fmap (daysOfMonth y) [1..(m-1)])
                + (d-1)

-- | Calculate the number of leap years between the two years (inclusive).
leapYears :: (Integer, Integer) -> Integer
leapYears (s,e) = leapYears' e - leapYears' (s-1)
  where
    leapYears' y = y `div` 4 + y `div` 400 - y `div` 100


daysOfMonth :: Year -> Month -> Integer
daysOfMonth y m
  | or (fmap (==m) [1,3,5,7,8,10,12]) = 31
  | or (fmap (==m) [4,6,9,11]) = 30
  | m == 2 && isLeapYear y = 29
  | m == 2 = 28
  | otherwise = error $ "Monnth not recognized: " ++ show m
