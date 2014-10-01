module Euler.P007
       ( solve
       ) where

import           Euler.Prime (primes)

solve :: Monad m => m Int
solve = return $ primes !! 10000 -- Index starting at 0!
