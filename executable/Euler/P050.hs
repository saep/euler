{- |
Module      :  Euler.P050
Description :  Problem 50
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Consecutive prime sum

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below
one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime,
contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most
consecutive primes?
-}
module Euler.P050
       ( solve
       ) where

import           Euler.Prime (isPrime)

solve :: Monad m => m Int
solve = return $ snd findLongestConsecutivePrime

primeStream :: [Int]
primeStream = 2 : [ x | x <- [3,5..], isPrime x ]

len :: (a, b) -> a
len = fst
acc :: (a, b) -> b
acc = snd

findLongestConsecutivePrime :: (Int, Int)
findLongestConsecutivePrime =
    uncurry (pick (0,0) (0,0)) . span ((< 1000000) . acc)
    . zip [1..] $ scanl1 (+) primeStream

-- mlen = maximum length
-- blen = length before window
-- bacc = sum before window
pick :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
pick m@(mlen, _) b@(blen, bacc) iv rs
    | (len . head) rs - blen + 1 < mlen = m
    | (acc . head) rs - bacc < 1000000 = pick m b (iv++[head rs]) (tail rs)
    | mlen' > mlen = pick (mlen', macc') (head iv) (tail iv) rs
    | otherwise = pick m (head iv) (tail iv) rs
        where
            (_,cps) = break (isPrime . subtract bacc . acc) $ reverse iv
            mlen' = max mlen $ (len . head) cps - blen
            macc' = (acc . head) cps - bacc
