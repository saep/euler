module Euler.P029
       ( solve
       ) where

import           Data.Bits
import qualified Data.IntSet as S

type BitSet = Integer

solve :: Monad m => m Int
-- This one works fast, but it is quadratic and this problem can be solved more elegantly.
-- solve = return . length . snub $ sort [ a^b | a <- [2..100], b <- [2..100] ]
solve = return $ distinctPowers 100 100

distinctPowers :: Int -> Int -> Int
distinctPowers nmax pmax =
  let maxCombinations = (nmax-1) * (pmax-1)
      ps = perfectPowerNumbers nmax pmax
      dupsForPowerNumbers (p,ns) = ns * duplicatesForPowerNumber nmax p
      allDups = sum $ fmap dupsForPowerNumbers ps
  in maxCombinations - allDups

-- | Calculate the numbers which are perfect powers.  A non perfect
-- power of 2 is 16. It is 4 to the power of 2, but 4 itself is 2 to
-- the power of 2. So 16 would be a perfect power of 4.
perfectPowerNumbers :: Int -> Int -> [(Int, Int)]
perfectPowerNumbers nmax pmax = go [p,p-1..2] 0
  where
    p :: Int
    p = (floor . logBase 2) (fromIntegral pmax :: Double)

    go :: [Int] -> BitSet -> [(Int, Int)]
    go (x:xs) bs = let pns = filter (not . testBit bs) . takeWhile (<=nmax) $ fmap (^x) [2..]
                   in (x, length pns) : go xs (foldl setBit bs pns)
    go _ _ = []

duplicatesForPowerNumber :: Int -> Int -> Int
duplicatesForPowerNumber nmax p =
  S.size $ foldl (\s l -> S.union s (S.fromAscList l)) S.empty
    [ fmap (*(x `div` gxp)) [2..gxp*nmax`div`p] | x <- [1..p-1], let gxp = gcd x p ]
