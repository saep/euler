module Euler.P001Spec
    ( spec ) where

import Euler.P001

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 001" $ do
           it "returns" $ do
               solve `shouldReturn` 233168
