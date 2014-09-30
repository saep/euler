module Euler.P008Spec
    ( spec ) where

import Euler.P008

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 008" $ do
           it "returns" $ do
               solve `shouldReturn` 40824
