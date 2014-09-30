module Euler.P018Spec
    ( spec ) where

import Euler.P018

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 018" $ do
           it "returns" $ do
               solve `shouldReturn` 1074
