module Euler.P007Spec
    ( spec ) where

import Euler.P007

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 007" $ do
           it "returns" $ do
               solve `shouldReturn` 104743
