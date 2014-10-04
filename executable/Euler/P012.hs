module Euler.P012
       ( solve
       ) where

import qualified Data.IntMap   as M
import           Data.List
import           Euler.Numbers (triangular)
import           Euler.Prime

solve :: Monad m => m Int
solve = return $ p12 1 [2..] M.empty

p12 :: Int -> [Int] -> M.IntMap [Int] -> Int
p12 dlast ~(x:xs) m
    | dlast * divisorsx > 500 = triangular (x-1)
    | otherwise = p12 divisorsx xs m'
  where
    fx = factors x
    p = head fx
    mx = maybe fx (insert p) $ M.lookup (x `div` p) m
    m' = M.insert x mx m
    effectiveFactors = if even x then tail mx else mx
    divisorsx = (product . fmap (succ . length) . group) effectiveFactors
