module Euler.P024Spec
    ( spec ) where

import Euler.P024

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 024" $ do
           it "returns" $ do
               solve `shouldReturn` 2783915460
