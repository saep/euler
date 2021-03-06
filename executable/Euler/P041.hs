module Euler.P041
       ( solve
       ) where

import           Data.List   (delete, (\\))
import           Euler.Prime

-- 5,6,8 and 9-pandigital numbers are always divisable by 3, so we can exclude
-- them we can also exclude 1 to 3-pandigital numbers as the example is greater
-- than that.
solve :: Monad m => m Int
solve = return $ head [ p | n <- "74", p <- pandigitalCandidates n ]

pandigitalCandidates :: Char -> [Int]
pandigitalCandidates n =
    filter isPrime [ read (x:xs++[l])
                   | x <- [n, pred n..'1']
                   , l <- filter (\e -> e <= n && e /= x) "731"
                   , xs <- pcs ([n, pred n..'1'] \\ [x,l])
                   ]
    where
        pcs [] = [[]]
        pcs ns = [ x:xs | x <- ns, xs <- pcs (delete x ns) ]
