module Euler.P042
       ( solve
       ) where

import           Control.Applicative
import           Data.Char

solve :: IO Int
solve = do
  ws <- read . (++"]") . filter (/= '\n') . ('[':) <$> readFile "text/words.txt"
  return . length $ filter isTriangularWord ws

wordSum :: String -> Int
wordSum = sum . fmap (subtract (ord 'A' - 1) . ord . toUpper)

{-
We know that if @w@ is a triangle word, then the equation
(wordSum w) == n(n+1)/2
must hold for some integer @n@. So we simply need to check, whether
@n@ is an integer that solves the equation.

    n*n + n = 2 * (wordSum w)
<=> (n+1/2)^2 = 2 * (wordSum w) + 1/4
<=> n + 1/2 = sqrt ((2 * wordSum w) + 1/4)
<=> n = sqrt ((2 * wordSum w) + 1/4) - 1/2
-}

isTriangularWord :: String -> Bool
isTriangularWord w =
  let ws = wordSum w
      n = floor $ sqrt ((2 * fromIntegral ws) + 0.25) - (0.5 :: Double)
  in n*n + n == 2 * ws
