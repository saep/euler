module Euler.P020
       ( solve
       ) where

import           Data.Char

solve :: Monad m => m Int
solve = return . sum . fmap digitToInt . show $ product [1..100]
