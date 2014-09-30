module Euler.P033Spec
    ( spec ) where

import Euler.P033

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 033" $ do
           it "returns" $ do
               solve `shouldReturn` 100
