module Euler.P012
       ( solve
       ) where

import           Data.List
import qualified Data.IntMap            as M
import           Euler.Formulae      (triangularNumber)
import           Euler.Prime

solve :: Monad m => m Int
solve = return $ p12 1 [2..] M.empty

p12 :: Int -> [Int] -> M.IntMap [Int] -> Int
p12 dlast (x:xs) m =
    let fx = factors x
        p = head fx
        mx = maybe fx (insert p) $ M.lookup (x `div` p) m
        m' = M.insert x mx m
        effectiveFactors = if even x then tail mx else mx
        divisorsx = (product . fmap (succ . length) . group) effectiveFactors
    in if dlast*divisorsx > 500
        then triangularNumber (x-1)
        else p12 divisorsx xs m'
