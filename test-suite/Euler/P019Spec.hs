module Euler.P019Spec
    ( spec ) where

import Euler.P019

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 019" $ do
           it "returns" $ do
               solve `shouldReturn` 171
