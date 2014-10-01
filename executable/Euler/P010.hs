module Euler.P010
       ( solve
       ) where

import           Euler.Prime (primes)

solve :: Monad m => m Int
solve = return . sum . fmap (fromIntegral :: Int -> Int)
        $ takeWhile (< 2000000) primes
