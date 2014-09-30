{- |
Module      :  Euler.P042
Description :  Problem 042
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Coded triangle numbers
Problem 42

The nth term of the sequence of triangle numbers is given by, t(n) =
½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word
value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
t(10). If the word value is a triangle number then we shall call the
word a triangle word.

Using words.txt, a 16K text file containing nearly two-thousand common
English words, how many are triangle words?

-}
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
