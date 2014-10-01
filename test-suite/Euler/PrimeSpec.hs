module Euler.PrimeSpec
       ( spec
       ) where

import           Euler.Prime

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = parallel $ do
    let atkinSieve = atkin 28123
        isPrimeA = isPrimeS atkinSieve
    describe "Prime test" $ do
        it "Atkin sieve" $ do
            map (\x -> isPrimeA x == isPrime x) [1..28123]
                `shouldSatisfy` and

        it "deterministic Miller-Rabin" $ do
            map (\x -> isPrimeMillerRabin (toInteger x) == isPrimeA x) [1..28123]
            `shouldSatisfy` and


    describe "Power of n modulo m" $ do
        it "should equal the naive method" $ property $ do
            \m b e ->
                let m' = getPositive m :: Integer
                    b' = getPositive b :: Integer
                    e' = getPositive e :: Integer
                in powMod m' b' e' == b'^e' `mod` m'

