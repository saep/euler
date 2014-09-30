module Euler.P042Spec
    ( spec ) where

import Euler.P042

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 042" $ do
           it "returns" $ do
               solve `shouldReturn` 162
