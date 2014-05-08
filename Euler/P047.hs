{- |
Module      :  Euler.P047
Description :  Problem 47
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Distinct primes factors

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors.
What is the first of these numbers?
-}
module Euler.P047
       ( solve
       ) where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Euler.Prime (sqrt')
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S

solve :: IO Int
solve = return . evalState (find [] 2) $ M.singleton 1 mempty

find :: [Int] -> Int -> State (IntMap IntSet) Int
find fs n = do
    fn <- lookupFactors n
    case () of
        _ | length fs == 4 -> return $ last fs
        _ | S.size fn == 4 -> find (n:fs) (n+1)
        _  -> find [] (n+1)

firstFactor :: Int -> Int
firstFactor n
    | even n = 2
    | otherwise = case dropWhile (\x -> n `mod` x /= 0) [3,5..sqrt' n] of
        []    -> n
        (x:_) -> x

lookupFactors :: Int -> State (IntMap IntSet) IntSet
lookupFactors n = do
    let f = firstFactor n
    fs <- maybe (S.singleton f) (S.insert f) . M.lookup (n `div` f) <$> get
    modify $ M.insert n fs
    return fs
