module Euler.P062Spec
    ( spec ) where

import Euler.P062

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 062" $ do
           it "returns" $ do
               solve `shouldReturn` 127035954683
