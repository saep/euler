module Euler.P001
       ( solve
       ) where

import           Euler.Formulae (sumTo)

solve :: Monad m => m Int
solve = return $ f 3 + f 5 - f (3*5)
  where
    limit = 999 :: Int
    f n = n * sumTo (limit `div` n)
