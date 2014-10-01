module Euler.P026
       ( solve
       ) where

import qualified Data.IntMap as M
import           Data.List   (maximumBy)

solve :: Monad m => m Int
solve = return . snd . maximumBy (\(x,_) (y,_) -> compare x y) $
        fmap (\x -> (reciprocalCycleLength x, x)) [999,998..1]

-- | The function d calculates the first number, for which @x@ is
-- greater than @n@. @n@ is the fraction for which we search the
-- length of the reciprocal cycle. With this helper function we
-- successively divide the remainder of the accumulating number by @n@
-- until we hit a remainder that we had before. The cycle length is
-- then the current index minus the index stored for that remainder.
reciprocalCycleLength :: Int -> Int
reciprocalCycleLength n = go M.empty 1 (d 1 `mod` n)
  where
    d x = head . dropWhile (<n) $ fmap (*x) [1,10..]

    go :: M.IntMap Int -> Int -> Int -> Int
    go rems i r
      | r == 0 = 0
      | otherwise = case M.lookup r rems of
        Just j -> i-j
        Nothing -> go (M.insert r i rems) (i+1) (d r `mod` n)
