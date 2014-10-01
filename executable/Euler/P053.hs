module Euler.P053
       ( solve
       ) where

-- c (n+1) r = (n+1)! / (r!(n+1-r)!)
-- = (n+1)n! / (r!(n+1-r)(n-r)!)
-- = (n+1)/(n+1-r) * n!/(r!(n-r)!)
-- = (n+1)/(n+1-r) * c n r
-- --> c (n+1) r ≥ c n r \forall 0 ≤ r ≤ n
--
-- c n (r-1) = n!/(r-1)!(n+1-r)!
-- = n!r/(r!(n+1-r)(n-r)!
-- = r/(n+1-r) * n!/(r!(n-r)!)
-- = r/(n+1-r) * c n r
--
-- c n (r+1) = n! / ((r+1)r!(n-1-r)!)
-- = (n-r)/(r+1) * c n r
--
solve :: Monad m => m Int
solve = return . sum $ f (23, 9, (1144066 * 5) `div` 7)

f :: (Int, Int, Int) -> [Int]
f (n,r,cnr)
    | n > 100 = []
    | cnr > 1000000 = f (n, r-1, cnr `div` (n+1-r) * r)
    | otherwise = n-2*r-1 : f (n+1,r,cnr `div` (n+1-r) * (n+1) )

