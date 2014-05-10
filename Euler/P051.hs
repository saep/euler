{- |
Module      :  Euler.P051
Description :  Problem 51
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Prime digit replacements

By replacing the 1st digit of the 2-digit number *3, it turns out that six of
the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
number is the first example having seven primes among the ten generated numbers,
yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest
prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily
adjacent digits) with the same digit, is part of an eight prime value family.

-}
module Euler.P051
       ( solve
       ) where

import           Control.Monad
import           Data.Char     (digitToInt)
import           Data.List     (foldl')
import           Data.Maybe
import           Euler.Prime   (isPrime)

type PatternNumber = String

solve :: IO Int
solve = return problem51

{- Observations
* The number of wildcars must be a multiple of 3 as otherwise you can only
  generate up to 7 numbers.
* The last digit must be one of [1,3,7,9].
-}

problem51 ::Int
problem51 = pick [ fromJust msol | n <- [4..]
                 , p <- [3,6..n-1]
                 , ls <- generate (n-p) p
                 , l <- "1379"
                 , let msol = test (l:ls)
                 , isJust msol
                 ]

pick :: [Int] -> Int
pick (x:xs) = minimum $ takeWhile (<stop) (x:xs)
    where
        stop = 10 ^ (ceiling . logBase 10 . fromIntegral) x

test :: PatternNumber -> Maybe Int
test p
    | last p == '*' = f 1 Nothing $ fmap (`patternToNum` p) [1..9]
    | otherwise = f 0 Nothing $ fmap (`patternToNum` p) [0..9]
        where
            f :: Int -> Maybe Int -> [Int] -> Maybe Int
            f nps m ns = case ns of
                _    | nps > 2 -> Nothing
                x:xs | (not . isPrime) x -> f (nps+1) m xs
                x:xs  -> f nps (m `mplus` Just x) xs
                _ -> m

patternToNum :: Int -> PatternNumber -> Int
patternToNum x = fst . foldl' f (0, 1)
    where
        f (acc, m) p
            | p == '*' = (acc + m * x, m*10)
            | otherwise = (acc + digitToInt p * m, m*10)

generate :: Int -> Int -> [PatternNumber]
generate n p
    | n == 0 = [ replicate p '*' ]
    | p == 0 = [ x:xs | x <- ['0'..'9'], xs <- generate (n-1) p ]
    | otherwise = [ '*':xs | xs <- generate n (p-1) ]
                    ++ [ x:xs | x <- ['0'..'9'], xs <- generate (n-1) p ]

