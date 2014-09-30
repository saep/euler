module Euler.P063Spec
    ( spec ) where

import Euler.P063

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 063" $ do
           it "returns" $ do
               solve `shouldReturn` 49
