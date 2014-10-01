module Euler.P005
       ( solve
       ) where

import           Euler.Formulae (gcf)

solve :: Monad m => m Int
solve = return . fromInteger  $ foldl1 (\a b -> a * b `div` gcf a b) [1..20]
