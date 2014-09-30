module Euler.P053Spec
    ( spec ) where

import Euler.P053

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 053" $ do
           it "returns" $ do
               solve `shouldReturn` 4075
