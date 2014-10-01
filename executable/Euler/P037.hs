module Euler.P037
       ( solve
       ) where

import           Control.Applicative
import           Euler.Prime         (isPrime)

-- | Even without limiting the number of resulting primes to 11, this
-- solution runs in less than a tenth of a second!
solve :: Monad m => m Int
solve = return . sum . take 11 $ filter isLeftToRightTruncateablePrime rightToLeftTruncateablePrimes

-- | This function does not create all primes which are truncateable
-- from right to left as it filters out the simple cases where any
-- digit may not be even or 5 as a left to right truncation would make
-- the number non-prime.
rightToLeftTruncateablePrimes :: [Int]
rightToLeftTruncateablePrimes = go (fmap next [2,3,5,7])
  where
    next :: Int -> [Int]
    next p = filter isPrime ((+) (p*10) <$> [1,3,7,9])

    go :: [[Int]] -> [Int]
    go (x:xs) = let cs = fmap next x in x ++ go (xs++cs)
    go _ = []

isLeftToRightTruncateablePrime :: Int -> Bool
isLeftToRightTruncateablePrime p = all isPrime . fmap (p `mod`) $ takeWhile (<p) (iterate (*10) 1)
