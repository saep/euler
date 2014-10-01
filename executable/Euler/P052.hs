module Euler.P052
       ( solve
       ) where

import           Data.List ((\\))

{-
As I've read in the forum to the solutions, the digits of the fraction 1/7
have the desired properties to solve this problem.
-}
solve :: Monad m => m Int
solve = return $ head [ x | x <- [10..]
                     , let sx = show x
                     , head sx == '1'
                     , head (tail sx) < '7'
                     , all (null . (\\ sx)) (fmap (show . (*x)) [2..6]) ]

