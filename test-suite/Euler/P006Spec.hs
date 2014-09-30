module Euler.P006Spec
    ( spec ) where

import Euler.P006

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 006" $ do
           it "returns" $ do
               solve `shouldReturn` 25164150
