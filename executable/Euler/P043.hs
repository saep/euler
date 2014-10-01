module Euler.P043
       ( solve
       ) where

import           Data.Char
import           Data.List (delete, foldl', (\\))

type RemainingCharSet = String

startValue :: [(String, RemainingCharSet)]
startValue = [ (x', set) | x <- [17,34..999] :: [Int]
             , let x' = if x < 100 then '0':show x else show x
             , let set = ['0'..'9'] \\ x'
             , length set == 7 ]

generate :: [Int]
generate = fmap (read . fst) . foldl' next startValue $ [13,11,7,5,3,2,1]

next :: [(String, RemainingCharSet)] -> Int -> [(String, RemainingCharSet)]
next cs d = [ (y:x', y `delete` s) | (x', s) <- cs
            , let x = read (take 2 x')
            , y <- s
            , (digitToInt y * 100 + x) `mod` d == 0
            ]

solve :: Monad m => m Int
solve = return $ sum generate
