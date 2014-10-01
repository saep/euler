module Euler.P058
       ( solve
       ) where

import Euler.Prime (isPrimeMillerRabin)

solve :: Monad m => m Int
solve = return . fst
        $ until (\(pl,ps) -> pl*2-1 > 10*ps ) nextSprialWithPrimePercentage (7, 8)

nextSprialWithPrimePercentage :: (Int,Int) -> (Int,Int)
nextSprialWithPrimePercentage (pl, pp) =
    let pl' = pl+2
        cns = filter isPrimeMillerRabin $ fmap (\i -> pl*pl + i*(pl+1)) [1,2,3]
        pp' = length cns + pp
    in (pl', pp')

