module Euler.P016Spec
    ( spec ) where

import Euler.P016

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 016" $ do
           it "returns" $ do
               solve `shouldReturn` 1366
