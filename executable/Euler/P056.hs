module Euler.P056
       ( solve
       ) where

import           Data.Char

solve :: Monad m => m Int
solve = return $ maximum [ digitSum (a^b) | a <- [1..99] , b <- [1..99] ]

digitSum :: Integer -> Int
digitSum = sum . fmap digitToInt . show

