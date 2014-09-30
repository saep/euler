module Euler.P002Spec
    ( spec ) where

import Euler.P002

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 002" $ do
           it "returns" $ do
               solve `shouldReturn` 4613732
