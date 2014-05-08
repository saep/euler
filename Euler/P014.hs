{- |
Module      :  Euler.P014
Description :  Problem 14
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Longest Collatz sequence

The following iterative sequence is defined for the set of positive
integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one
million.

-}
module Euler.P014
       ( solve
       ) where


import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

solve :: IO ()
solve = print $ V.maxIndex collatzVector

collatz :: Int -> [Int]
collatz n
  | n == 1 = [1]
  | odd n = n : collatz (3*n + 1)
  | otherwise = n : collatz (n `div` 2)

collatzVector :: V.Vector Int
collatzVector = V.create $ do
    v <- VM.new 1000000
    VM.write v 0 0
    VM.write v 1 1
    forM_ [2..999999] $ \i -> do
            let (ns, old:_) = span (>= i) $ collatz i
            lengthOld <- VM.read v old
            VM.write v i (length ns + lengthOld)
    return v

