module Euler.P003Spec
    ( spec ) where

import Euler.P003

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 003" $ do
           it "returns" $ do
               solve `shouldReturn` 6857
