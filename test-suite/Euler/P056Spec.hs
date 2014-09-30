module Euler.P056Spec
    ( spec ) where

import Euler.P056

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 056" $ do
           it "returns" $ do
               solve `shouldReturn` 972
