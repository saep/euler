module Euler.P046
       ( solve
       ) where

import           Euler.Prime

solve :: Monad m => m Int
solve = return $ until (not . testProperty) (+2) 9

testProperty :: Int -> Bool
testProperty c =
    let dsqs = reverse . takeWhile (<c) $ fmap (\n -> 2*n*n) [1..]
    in isPrime c || any (isPrime . (c-)) dsqs
