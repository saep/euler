module Euler.P021
       ( solve
       ) where

import           Euler.Numbers

solve :: Monad m => m Int
solve = let limit = 9999
        in return . sum $ evalState (amicableNumbersTo limit) (initialWithDivisorsState limit)
