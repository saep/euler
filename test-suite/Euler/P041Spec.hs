module Euler.P041Spec
    ( spec ) where

import Euler.P041

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 041" $ do
           it "returns" $ do
               solve `shouldReturn` 7652413
