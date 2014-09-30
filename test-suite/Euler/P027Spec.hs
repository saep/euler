module Euler.P027Spec
    ( spec ) where

import Euler.P027

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 027" $ do
           it "returns" $ do
               solve `shouldReturn` -59231
