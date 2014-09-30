module Euler.P037Spec
    ( spec ) where

import Euler.P037

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 037" $ do
           it "returns" $ do
               solve `shouldReturn` 748317
