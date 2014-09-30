module Euler.P059Spec
    ( spec ) where

import Euler.P059

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 059" $ do
           it "returns" $ do
               solve `shouldReturn` 107359
