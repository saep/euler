module Euler.P039
       ( solve
       ) where

import           Control.Arrow ((&&&))
import           Data.List     (group, sort)
import           Euler.Numbers (pythagoreanTriplets)

solve :: Monad m => m Int
solve = return . snd . maximum
        . fmap (length &&& head)
        . group . sort
        . concat . takeWhile (not . null)
        . fmap (takeWhile (<= 1000))
        $ fmap (fmap (\(a,b,c) -> a+b+c)) pythagoreanTriplets
