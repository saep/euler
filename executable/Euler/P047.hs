module Euler.P047
       ( solve
       ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as M
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as S
import           Data.Monoid
import           Euler.Prime         (sqrt')

solve :: Monad m => m Int
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
