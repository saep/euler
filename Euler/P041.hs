{- |
Module      :  Euler.P041
Description :  Problem 041
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Pandigital prime

We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once. For example, 2143 is a 4-digit
pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

-}
module Euler.P041
       ( solve
       ) where

import Data.List (sort, delete, (\\))
import Euler.Prime

-- 5,6,8 and 9-pandigital numbers are always divisable by 3, so we can exclude them
-- we can also exclude 1 to 3-pandigital numbers as the example is greater than that.
solve = print $ head [ p | n <- "74", p <- pandigitalCandidates n ]

pandigitalCandidates :: Char -> [Int]
pandigitalCandidates n = filter isPrime [ read (x:xs++[l])
                                        | x <- [n, pred n..'1']
                                        , l <- filter (\e -> e <= n && e /= x) "137"
                                        , xs <- pcs ([n, pred n..'1'] \\ [x,l])
                                        ]
  where
    pcs [] = [[]]
    pcs ns = [ x:xs | x <- ns, xs <- pcs (delete x ns) ]
