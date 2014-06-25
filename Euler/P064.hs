{- |
Module      :  Euler.P064
Description :  Problem 64
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

-}
module Euler.P064
       ( solve
       ) where

import           Euler.Numbers (continuedFractions)
import           Euler.SList

square :: Int -> Int
square x = x*x

solve :: Monad m => m Int
solve = return . length . filter (odd . length . snd)
        $ map continuedFractions ([2..10000] `sremoveAll` fmap square [1..])

