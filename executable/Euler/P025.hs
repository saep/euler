module Euler.P025
       ( solve
       ) where

import           Euler.Formulae

solve :: Monad m => m Int
solve = let threshold = 10^999
        in return . fst . head $ dropWhile (\(_, n) -> n - threshold < 0) $ zip [0..] fibs
