{- |
Module      :  Euler.P064
Description :  Problem 64
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

-}
module Euler.P064
       ( solve
       ) where

import           Euler.Prime (sqrt')
import           Euler.SList

square :: Int -> Int
square x = x*x

solve :: Monad m => m Int
solve = return . length . filter (odd . length . snd)
        $ map continuedFractions ([2..10000] `sremoveAll` fmap square [1..])

continuedFractions :: Int -> ([Int], [Int])
continuedFractions s = go [] $ iterate next (0,1,a0)
  where
    a0 = sqrt' s

    _3 (_,_,a) = a

    go :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([Int], [Int])
    go cs ~(x:xs)
        | x `elem` cs = (\(n,p) -> (map _3 n, map _3 p))
                        . span (/=x) $ reverse cs
        | otherwise = go (x:cs) xs


    next :: (Int, Int, Int) -> (Int, Int, Int)
    next (m,d,a) = let m' = d * a - m
                       d' = (s - m' * m') `div` d
                       a' = (a0 + m') `div` d'
                   in (m',d',a')
