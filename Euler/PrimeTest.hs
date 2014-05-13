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
       , powModEqualsNaiveMethod
       ) where

import           Euler.Prime

import           Test.HUnit
import           Test.QuickCheck

tests :: Test
tests = TestList [ TestLabel "Atkin test " testAtkin
                 , TestLabel "Miller rabin primality test " testMillerRabin
                 ]

atkinSieve :: PrimeSieve
atkinSieve = atkin 28123

isPrimeA :: Int -> Bool
isPrimeA = isPrimeS atkinSieve

testAtkin :: Test
testAtkin =
    TestCase $
        mapM_ (\(x,b) -> assertBool ("prime test failed for : " ++ show x) b)
            [ (x,isPrimeA x == isPrime x) | x <- [1..28123] ]

testMillerRabin :: Test
testMillerRabin = TestCase $
    mapM_ (\(x,b) -> assertBool ("prime test failed for: " ++ show x) b)
        [ (x,isPrimeMillerRabin (toInteger x) == isPrimeA x) | x <- [1..28123] ]

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

powModEqualsNaiveMethod :: Positive Integer
                        -> Positive Integer
                        -> Positive Integer
                        -> Bool
powModEqualsNaiveMethod m b e =
    let m' = fi m
        b' = fi b
        e' = fi e
    in powMod m' b' e' == b'^e' `mod` m'

