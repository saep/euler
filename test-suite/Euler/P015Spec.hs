module Euler.P015Spec
    ( spec ) where

import Euler.P015

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 015" $ do
           it "returns" $ do
               solve `shouldReturn` 137846528820
