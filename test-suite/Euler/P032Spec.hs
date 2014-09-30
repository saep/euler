module Euler.P032Spec
    ( spec ) where

import Euler.P032

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 032" $ do
           it "returns" $ do
               solve `shouldReturn` 45228
