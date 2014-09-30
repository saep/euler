module Euler.P060Spec
    ( spec ) where

import Euler.P060

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 060" $ do
           it "returns" $ do
               solve `shouldReturn` 26033
