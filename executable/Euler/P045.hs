module Euler.P045
       ( solve
       ) where

-- Since every hexagonal number is also a triangular number, it suffices to
-- search for a pentagonal number that is also a hexagonal number.
solve :: Monad m => m Int
solve = return . head . dropWhile (<= 40755)
        $ keepSame (scanl1 (+) [1,4..]) (scanl1 (+) [1,5..])

keepSame :: [Int] -> [Int] -> [Int]
keepSame (p:ps) (h:hs)
    | p < h = keepSame ps (h:hs)
    | p > h = keepSame (p:ps) hs
    | otherwise = p : keepSame ps hs

{-
p(n) = t(3n-1) / 3

t*t+t = 2x

3p^2 - p = 2x
2h^2 - h = x
--------------------
3p^2 - p = 4h^2 - 2h
<=> (2h - 1/2)^2 - 1/4 = 3p^ - p
 => 2h -1/2 = sqrt (3p^2 -p + 1/4) | negative h is not defined
 => h = 1/4 + sqrt (3p^2 - p + 1/4) / 2


p(n+1) = (3(n+1)^2 - (n+1)) /2
       = (3n^2 + 6n + 3 - n - 1) /2
       = (3n^2 - n)/2 + 3n + 1

=> sum_{i=0}^{n-1} 3*i + 1

h(n+1) = 2(n+1)^2 - (n+1)
       = 2n^ - n + 4n + 1

=> sum_{i=0}^{n-1} 4*i + 1
-}
