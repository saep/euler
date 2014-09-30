module Euler.P030Spec
    ( spec ) where

import Euler.P030

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 030" $ do
           it "returns" $ do
               solve `shouldReturn` 443839
