module Euler.P011Spec
    ( spec ) where

import Euler.P011

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 011" $ do
           it "returns" $ do
               solve `shouldReturn` 70600674
