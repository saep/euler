module Euler.P051Spec
    ( spec ) where

import Euler.P051

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 051" $ do
           it "returns" $ do
               solve `shouldReturn` 121313
