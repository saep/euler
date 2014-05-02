{- |
Module      :  Euler.P049
Description :  Problem 49
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Prime permutations

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
by 3330, is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence?
-}
module Euler.P049
       ( solve
       ) where

import Control.Monad.State
import Euler.Prime (isPrime)
import Data.List (permutations)
import Data.IntSet (IntSet, member)
import qualified Data.IntSet as S

solve :: IO ()
solve = let initialState = (S.fromList . fmap read . permutations . show) 8147
        in print $ evalState (findNumber [9997,9995..1001]) initialState

findNumber :: [Int] -> State IntSet String
findNumber ~(x:xs) = do
    s <- get
    case () of
        _ | x `member` s -> findNumber xs
        _ -> maybe (findNumber xs) return =<< lookupSequence x


lookupSequence :: Int -> State IntSet (Maybe String)
lookupSequence n = do
    let ps = (S.fromList . fmap read . permutations . show) n
    modify $ flip S.union ps
    let ps' = S.filter (\x -> (x>1000) && isPrime x) ps
    if S.size ps' < 3
        then return Nothing
        else return $ findSuitableDifference ps' (S.toDescList ps')

findSuitableDifference :: IntSet -> [Int] -> Maybe String
findSuitableDifference ps (x:xs) = case xs of
    [] -> Nothing
    (y:_) | (2*y -x) `member` ps -> Just $ concatMap show [2*y-x,y,x]
    _ -> case findSuitableDifference ps (x:(tail xs)) of
        Nothing -> findSuitableDifference ps xs
        ret -> ret

