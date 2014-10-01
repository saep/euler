module Euler.P015
       ( solve
       ) where

-- | (a+b)! /(a! * b!)
solve :: Monad m => m Int
solve = return . fromInteger $ (fac (20+20) `div` (fac 20 * fac 20))

fac :: Integer -> Integer
fac n = product [1..n]
