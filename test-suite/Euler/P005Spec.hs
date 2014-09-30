module Euler.P005Spec
    ( spec ) where

import Euler.P005

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 005" $ do
           it "returns" $ do
               solve `shouldReturn` 232792560
