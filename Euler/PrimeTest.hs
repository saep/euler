{- |
Module      :  Euler.PrimeTest
Description :  Test cases for the Euler.Prime module
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

-}
module Euler.PrimeTest
       ( tests
       ) where

import qualified Data.Vector.Unboxed as V
import           Euler.Prime

import           Test.HUnit

tests :: Test
tests = TestList [ TestLabel "Atkin test " testAtkin
                 , TestLabel "Miller rabin primality test " testMillerRabin
                 ]

testAtkin :: Test
testAtkin =
    let a = atkin 28123
        isPrimeA n = a V.! n
    in TestCase $
        mapM_ (\(x,b) -> assertBool ("prime test failed for : " ++ show x) b)
            [ (x,isPrimeA x == isPrime x) | x <- [1..28123] ]

testMillerRabin :: Test
testMillerRabin = TestCase $
    mapM_ (\(x,b) -> assertBool ("prime test failed for: " ++ show x) b)
        [ (x,isPrimeMillerRabin x == isPrime x) | x <- [1..28123] ]
