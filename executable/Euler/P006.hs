module Euler.P006
       ( solve
       ) where

import           Euler.Formulae (squareOfSum, sumTo)

solve :: Monad m => m Int
solve = return . abs $ squareOfSum 100 - sumTo 100^2
