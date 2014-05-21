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

import qualified Data.Vector.Unboxed as V

import Euler.Prime

import Control.Applicative
import Control.Monad.State

data BranchAndBound  = BB
                     { currentMin         :: Int
                     , currentMinSolution :: [Int]
                     , primeBound         :: Int
                     , primes'            :: [Int]
                     , sieve              :: PrimeSieve
                     }

type Branch = State BranchAndBound

solve :: Monad m => m Int
solve = return . sum $ findConcattablePrimeSet 5

findConcattablePrimeSet :: Int -> [Int]
findConcattablePrimeSet n = evalState (findPrimes n) initialBB

initialBB :: BranchAndBound
initialBB = BB
          { currentMin         = maxBound
          , currentMinSolution = []
          , primeBound         = 10000
          , primes'            = [ x | x <- ([3,5..primeBound initialBB])
                                 , isPrimeS (sieve initialBB) x ]
          , sieve              = atkin 10000
          }

instance Show BranchAndBound where
    show bb = show (currentMin bb) ++ " " ++ show (currentMinSolution bb)


findPrimes :: Int -> Branch [Int]
findPrimes n = do
    bb <- get
    case primes' bb of
        [] -> error "TODO could not find a solution for the current bound."
        (p:_) | n*p > currentMin bb -> return (currentMinSolution bb)
        (p:ps) -> do
            findNPairs (n-1) [p] ps
            modify $ \s -> s { primes' = ps }
            findPrimes n

findNPairs :: Int -> [Int] -> [Int] -> Branch ()
findNPairs n pairs ps = do
    bb <- get
    case [ x | x <- takeWhile (\p -> p*n < (currentMin bb)) ps, all (isConcattablePrimePair x) pairs ] of
        [] -> return ()
        x:_ | n == 1 && sum (x:pairs) < currentMin bb ->
            modify $ \s -> s { currentMin = sum (x:pairs), currentMinSolution = (x:pairs) }

        xs -> let ps' x = [ p | p <- [x+2,x+4..primeBound bb], isPrimeS (sieve bb) p ]
              in sequence_ $ map (\x -> findNPairs (n-1) (x:pairs) (ps' x)) xs

isConcattablePrimePair :: Int -> Int -> Bool
isConcattablePrimePair a b =
    let exb = until (>a) (*10) 10
        exa = until (>b) (*10) 10
    in isPrime' (exa*a+b) && isPrime' (exb*b+a)

isConcattablePrime :: (Show a1, Show a) => a1 -> a -> Bool
isConcattablePrime p1 p2 = (isPrime' . read) (show p1 ++ show p2) && (isPrime' . read) (show p2 ++ show p1)


