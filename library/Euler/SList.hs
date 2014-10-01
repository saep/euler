module Euler.SList
       ( sremoveAll
       , sremoveFirst
       , snub
       , sunion
       , module Data.List
       ) where

import           Data.List (insert, sort)

sremoveAll :: Ord a => [a] -> [a] -> [a]
sremoveAll (x:xs) (y:ys) = case compare x y of
  LT -> x : sremoveAll xs (y:ys)
  EQ -> sremoveAll xs (y:ys)
  GT -> sremoveAll (x:xs) ys
sremoveAll xs _ = xs

snub :: Ord a => [a] -> [a]
snub (x:y:ys)
  | x == y = snub (y:ys)
  | otherwise = x : snub (y:ys)
snub xs = xs

sunion :: Ord a => [a] -> [a] -> [a]
sunion (x:xs) (y:ys) = case compare x y of
  LT -> x : sunion xs (y:ys)
  EQ -> x : sunion xs ys
  GT -> y : sunion (x:xs) ys
sunion xs [] = xs
sunion [] ys = ys

sremoveFirst :: Ord a => a -> [a] -> [a]
sremoveFirst _ [] = []
sremoveFirst r (x:xs)
    | r < x     = x:sremoveFirst r xs
    | r > x     = x:xs
    | otherwise = xs
