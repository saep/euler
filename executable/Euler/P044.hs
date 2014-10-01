module Euler.P044
       ( solve
       ) where

import           Data.IntSet   (IntSet, insert, member)
import           Data.Maybe
import           Data.Monoid
import           Euler.Numbers (pentagonal)

solve :: Monad m => m Int
solve = return solution

solution :: Int
solution = uncurry subtract $ findPair mempty [] [1..]

findPair :: IntSet -> [Int] -> [Int] -> (Int, Int)
findPair m ps (z:zs) =
    let pz = pentagonal z
        m' = insert pz m
    in fromMaybe (findPair m' (pz:ps) zs) $ findD m' ps pz

-- p(z) = p(j) + p(k) <=> p(j) = p(z) - p(k)
findD :: IntSet -> [Int] -> Int -> Maybe (Int, Int)
findD _ [] _ = Nothing
findD m (pk:pks) pz
    | 2*pk < pz = Nothing
    | (2*pk - pz) `member` m && (pz - pk) `member` m = Just (pz - pk, pk)
    | otherwise = findD m pks pz

-- p(z) - p(k) = p(j)
-- p(k) - p(j) = p(k) - p(z) + p(k) = 2p(k) - p(z)


