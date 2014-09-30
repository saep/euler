module Euler.P064Spec
    ( spec ) where

import Euler.P064

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 064" $ do
           it "returns" $ do
               solve `shouldReturn` 1322
