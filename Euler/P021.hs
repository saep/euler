{- |
Module      :  Euler.P021
Description :  Problem 021
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less
than n which divide evenly into n).  If d(a) = b and d(b) = a, where a
≠ b, then a and b are an amicable pair and each of a and b are called
amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284
are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

-}
module Euler.P021
       ( solve
       ) where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Euler.Prime
import Euler.SList
import Data.List hiding (union)
import qualified Data.IntMap as M

type WithDivisors = State (M.IntMap [Int])

solve :: IO ()
solve = print . sum $ evalState (amicableNumbersTo 9999) (M.singleton 1 [])

amicableNumbersTo :: Int -> WithDivisors [Int]
amicableNumbersTo n = filterM isAmicable ([2..n] `sremoveAll` primes)

isAmicable :: Int -> WithDivisors Bool
isAmicable n = do
  dn <- sumDivisors n
  if n == dn
    then return False
    else (==n) <$> sumDivisors dn

sumDivisors :: Int -> WithDivisors Int
sumDivisors n = get >>= \m -> case M.lookup n m of
  Just xs -> return $ sum xs
  Nothing -> modify (mkDivisors n) >> sumDivisors n

mkDivisors :: Int -> M.IntMap [Int] -> M.IntMap [Int]
mkDivisors n m
  | n == fd      = M.insert n [1] m
  | isNothing pd = mkDivisors n $ mkDivisors p m
  | otherwise =
    let pd' = fromJust pd ++ [p]
        -- new divisors
        nd = filter (\x -> x <= n && 0 ==  n `mod` x) $ fmap (*fd) pd'
        -- proper divisors of n
        pdn = pd' `sunion` insert fd nd
    in M.insert n ((init . snub) pdn) m

  where
    fd = (head . factors) n -- first divisor
    p = n `div` fd          -- previous
    pd = M.lookup p m       -- previous divisors
