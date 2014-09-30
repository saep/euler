{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Euler.P031
Description :  Problem 031
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Coin sums

In England the currency is made up of pound, £, and pence, p, and
there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?

-}
module Euler.P031
       ( solve
       ) where

import           Data.List (group, sort)

type Coin = Int

solve :: Monad m => m Int
solve = return . fromInteger  $ coinBinations 200 [1,2,5,10,20,50,100,200]

-- | This problem can be solved by using dynamic programming. The
-- general idea is to calculate the possible combinations for the
-- smallest coin and reuse that to calculate those for the next
-- coin.
coinBinations :: Int -> [Coin] -> Integer
coinBinations n coins =
  let scoins = (takeWhile (<=n) . fmap head . group . sort) coins
  in last $ foldl dynCoinbinations (1:replicate n 0) scoins

dynCoinbinations :: [Integer] -> Coin -> [Integer]
dynCoinbinations ls c = keep ++ go keep calculate []
  where
    (keep, calculate) = splitAt c ls

    go _ [] rs = reverse rs
    go [] xs rs = let rrs = reverse rs in rrs ++ go rrs xs []
    go (k:ks) (x:xs) rs = go ks xs ((k+x):rs)
