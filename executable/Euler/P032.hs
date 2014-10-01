module Euler.P032
       ( solve
       ) where

import           Data.Bits
import           Data.List     (group, nub, sort, (\\))
import           Euler.Numbers (toDigits, toNum)

solve :: Monad m => m Int
solve = return . sum . fmap head . group . sort $ pandigitalProducts 9

pandigitalProducts :: Int -> [Int]
pandigitalProducts n = fmap (\(a,b,_) -> a*b) . filter isPandigital $ candidates n

isPandigital :: (Int, Int, [Int]) -> Bool
isPandigital (a,b,rs) = (rs==) . sort $ toDigits (a*b)

-- | Truncate the possibilities to try by looking at the indexes at
-- which the possible solutions are split.
--
-- i < j : This would collide with the parition function and should
-- have been covered before when i's and j's value were swapped.
--
-- The highest possible digit count for the multiplication of two
-- numbers i,j is (digits i)+(digits k) and the smallest possible
-- digit count is (digits i)+(digits k). (Just take 10*10 and 99*99 as
-- an example for this). And since k = j-i the following conditions
-- must be fulfilled.
--
-- hasValidDimensions :: Int -> Int -> Int -> Bool
-- hasValidDimensions i j n = i < j && (2*j == n || n == 2*j-1)

validDimensions :: Int -> [(Int, Int)]
validDimensions n = let js = nub [n `div` 2, (n+1) `div` 2]
                    in [ (i,j) | j <- js, i <- [1..j-1], j-i >= i ]

partition :: Int -> Int -> [Int] -> (Int, Int, [Int])
partition i j xs = let (a,r) = splitAt i xs
                       (b,c) = splitAt (j-i) r
                   in (toNum a, toNum b,c)

candidates :: Int -> [(Int, Int, [Int])]
candidates n = [ partition i j (ps++([1..n] \\ ps))
               | (i,j) <- validDimensions n
               , ps <- perms j [1..n] 0 ]

perms :: Int -> [Int] -> Int -> [[Int]]
perms n ns bs
  | n <= 0 = [[]]
  | otherwise = [ x:xs | x <- ns, (not . testBit bs) x, xs <- perms (n-1) ns (setBit bs x) ]
