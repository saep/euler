module Euler.Formulae where

import Data.Bits
import Data.List

-- | > sum [1..n]
sumTo :: Integral n => n -> n
sumTo n = n * (n+1) `div` 2

-- | Infinite Fibonacci sequence.
fibs :: [Integer]
fibs = scanl (+) 0 (1:fibs)

-- | The n-th Fibonacci number (starting with 1).
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
        [ testBit n k | k <- let s = bitSize n in [s-1, s-2 .. 0] ]
  where
    fib' (f, g) p
      | p         = (f * (f + 2*g), ss)
      | otherwise = (ss, g * (2*f - g))
      where ss = f*f + g*g
