module Euler.P016
       ( solve
       ) where

import           Data.Char

solve :: Monad m => m Int
solve = return . sum . fmap digitToInt . show $ 2^1000
