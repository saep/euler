module Euler.P021Spec
    ( spec ) where

import Euler.P021

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 021" $ do
           it "returns" $ do
               solve `shouldReturn` 31626
