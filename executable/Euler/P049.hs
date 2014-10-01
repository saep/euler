module Euler.P049
       ( solve
       ) where

import           Control.Monad.State
import           Data.IntSet         (IntSet, member)
import qualified Data.IntSet         as S
import           Data.List           (permutations)
import           Euler.Prime         (isPrime)

solve :: Monad m => m Int
solve = let initialState = (S.fromList . fmap read . permutations . show) 8147
        in return . read $ evalState (findNumber [9997,9995..1001]) initialState

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

