{- |
Module      :  Euler.P027
Description :  Problem 027
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Quadratic primes

Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the
consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 =
40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² +
41 + 41 is clearly divisible by 41.

The incredible formula n² − 79n + 1601 was discovered, which produces
80 primes for the consecutive values n = 0 to 79. The product of the
coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.

-}
module Euler.P027
       ( solve
       ) where

import Control.Monad.State
import Data.Bits
import Data.List (maximumBy)
import Euler.Prime

-- Each b must definitively be prime as for n = 0, the function can
-- only return primes if b is prime itself.
--
-- Except for the case of b=2, a has to be odd. This is true because
-- the result of a sum with an odd amount of summands can only be odd
-- (and hence have a chance of being prime for antyhing else than 2)
-- if an even amount of them is even. In this case only two or none of
-- the summands may be even.  In the special case of b=2, a must
-- therefore be even.
--
-- Since all primes are greater than 1, the following property must
-- hold as well: 1 + a + b > 1 <=> a > -b
solve :: IO ()
solve = let bs = takeWhile (< 1000) primes
            coefficients = [ (a,b) | b <- tail bs, a <- [-b+2,-b+4..999] ] ++ [ (a,2) | a <- [-4,-6..998] ]
            generatedPrimes = evalState (generatePrimes coefficients) (0, primes)
        in print . (\(_,a,b) -> a*b)
           $ maximumBy (\(x,_,_) (y,_,_) -> compare (length x) (length y)) generatedPrimes

generatePrimes :: [(Int,Int)] -> State (Integer, [Int]) [([Int], Int, Int)]
generatePrimes = mapM go
  where
    go (a,b) = do
      ps <- takeWhileM isPrime $ fmap (\n -> n*n + a*n + b) [0..]
      return (ps, a, b)

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM mp (x:xs) = do
  p <- mp x
  if p
    then return . (x:) =<< takeWhileM mp xs
    else return []

takeWhileM _ [] = return []

isPrime :: Int -> State (Integer, [Int]) Bool
isPrime n = do
  (bs, ps) <- get
  let (sps, bps) = span (<=n) ps
      bs' = foldl setBit bs sps
  when ((not . null) sps) $ put (bs', bps)
  return $ testBit bs' n
