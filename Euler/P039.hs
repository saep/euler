{- |
Module      :  Euler.P039
Description :  Problem 039
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Integer right triangles

If p is the perimeter of a right angle triangle with integral length
sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

-}
module Euler.P039
       -- ( solve
       -- ) where
       where

import Data.IntMap
import qualified Data.IntMap as M
import Data.List
import Data.Maybe

solve :: IO ()
solve = print . snd . maximum . fmap (\p -> (length p, head p)) . group . sort $ ps

feasibleSquares :: [(Int, Int)]
feasibleSquares = zip (takeWhile (<= 500*500) (fmap (\c -> c*c) [1..])) [1..]

feasibleSquareMap :: IntMap Int
feasibleSquareMap = M.fromList feasibleSquares

{- Observations

a*a + b*b = c*c
=> a+b+c is even

a+b+c = p <=> c = p - a - b
a+b+c <= 1000

a*a + b*b = p*p + a*a + b*b + 2*a*b - 2*a*p - 2*b*p
<=> p*p + 2*a*b - 2*a*p - 2*b*p = 0
<=> 2*a*b - 2*b*p = 2*a*p - p*p
<=> a*b - b*p = a*p - p*p/2
<=> b*(a - p) = p*(a-p/2)
<=> b = p*(a-p/2)/(a-p)

a+b > c edges of the triangle would not connect otherwise
=> a+b+c <= 1000 <=> 2*c <= 1000 <=> c <= 500
-}

ps :: [Int]
ps = [ a+b+fromJust c | (aa,a) <- feasibleSquares
                      , (bb,b) <- dropWhile ((<aa) . fst) feasibleSquares
                      , let c = M.lookup (aa+bb) feasibleSquareMap
                      , isJust c
                      ]
