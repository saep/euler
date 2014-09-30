module Euler.P052Spec
    ( spec ) where

import Euler.P052

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 052" $ do
           it "returns" $ do
               solve `shouldReturn` 142857
