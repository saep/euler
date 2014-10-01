module Euler.P027
       ( solve
       ) where

import           Control.Monad.State
import           Data.Bits
import           Data.List           (maximumBy)
import           Euler.Prime         hiding (isPrime)

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
solve :: Monad m => m Int
solve = let bs = takeWhile (< 1000) primes
            coefficients = [ (a,b) | b <- tail bs, a <- [-b+2,-b+4..999] ] ++ [ (a,2) | a <- [-4,-6..998] ]
            generatedPrimes = evalState (generatePrimes coefficients) (0, primes)
        in return . (\(_,a,b) -> a*b)
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
  unless (null sps) $ put (bs', bps)
  return $ testBit bs' n
