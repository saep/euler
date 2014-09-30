module Euler.P058Spec
    ( spec ) where

import Euler.P058

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 058" $ do
           it "returns" $ do
               solve `shouldReturn` 26241
