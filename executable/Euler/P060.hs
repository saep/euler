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
