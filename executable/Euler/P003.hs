module Euler.P003
       ( solve
       ) where

import           Euler.Prime

solve :: Monad m => m Int
solve = return . maximum $ factorizeSingleNumber 600851475143
