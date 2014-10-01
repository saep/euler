module Euler.P002
       ( solve
       ) where

import           Euler.Formulae

solve :: Monad m => m Int
solve = return . fromInteger . sum . filter even $ takeWhile (< 4000000) fibs
