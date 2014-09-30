module Euler.P009Spec
    ( spec ) where

import Euler.P009

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 009" $ do
           it "returns" $ do
               solve `shouldReturn` 31875000
