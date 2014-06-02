{- |
Module      :  Euler.P060
Description :  Problem 60
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Prime pair sets

The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
and concatenating them in any order the result will always be prime. For
example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four
primes, 792, represents the lowest sum for a set of four primes with this
property.

Find the lowest sum for a set of five primes for which any two primes
concatenate to produce another prime.

-}
module Euler.P060
       ( solve
       ) where

import Euler.Prime

solve :: Monad m => m Int
solve = return . sum $ findNPrimePairs 5

findNPrimePairs :: Int -> [Int]
findNPrimePairs n =
    let ms = iterate (*10) 10000
    in head $ concatMap (\m -> nPrimePair (takeWhile (<m) primes) n) ms

isPrimePair :: Int -> Int -> Bool
isPrimePair x y =
    let bx = until (> y) (*10) 10
        by = until (> x) (*10) 10
    in isPrime' (x + y*by) && isPrime' (y + x*bx)

nPrimePair :: [Int] -> Int -> [[Int]]
nPrimePair _ 0 = [[]]
nPrimePair [] _ = []
nPrimePair (p:ps) n = [ p:xs | let nps = filter (isPrimePair p) ps
                      , xs <- nPrimePair nps (n-1) ] ++ nPrimePair ps n
