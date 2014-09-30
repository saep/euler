module Euler.P057Spec
    ( spec ) where

import Euler.P057

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 057" $ do
           it "returns" $ do
               solve `shouldReturn` 153
