module Euler.P028
       ( solve
       ) where

-- diagonalSum (2*k+1) = 1 + Σ_(i=1)^k 1 + 4(2i+1)^2 - 12*i
-- = 1 + Σ_(i=1)^k 16i^2 + 4i + 4
-- = 1 + 16k(k+1)(2k+1)/6 + 4k(k+1)/2 + Σ_(k=1)^k 4
-- = 1 + 8k(k+1)(2k+1)/3 + 2k(k+1) + 4*k

solve :: Monad m => m Int
solve = return $ diagonalSum 1001

diagonalSum :: Int -> Int
diagonalSum n = let k = (n-1) `div` 2
                in 1 + (8*k*(k+1)*(2*k+1)) `div` 3 + 2*k*(k+1) + 4*k


