module Euler.P047Spec
    ( spec ) where

import Euler.P047

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 047" $ do
           it "returns" $ do
               solve `shouldReturn` 134043
