module Euler.P034Spec
    ( spec ) where

import Euler.P034

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 034" $ do
           it "returns" $ do
               solve `shouldReturn` 40730
