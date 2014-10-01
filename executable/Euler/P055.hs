module Euler.P055
       ( solve
       ) where

solve :: Monad m => m Int
solve = return . length $ filter (not . isLychrel) [6..9999]


isLychrel :: Integer -> Bool
isLychrel = (/= 50) . fst
            . until (\(c,n) -> (isPalindrome . show) n || c >= 50) f . f . (,) 0
    where
        f (c,n) = (c+1, n + (read . reverse . show) n)

isPalindrome :: String -> Bool
isPalindrome xs = let lxs = length xs `div` 2
                  in all (uncurry (==)) . zip xs $ (take lxs . reverse) xs


