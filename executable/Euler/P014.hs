module Euler.P014
       ( solve
       ) where


import           Control.Monad
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

solve :: Monad m => m Int
solve = return $ V.maxIndex collatzVector

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

