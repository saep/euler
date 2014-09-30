module Euler.P036Spec
    ( spec ) where

import Euler.P036

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 036" $ do
           it "returns" $ do
               solve `shouldReturn` 872187
