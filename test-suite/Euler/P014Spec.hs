module Euler.P014Spec
    ( spec ) where

import Euler.P014

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 014" $ do
           it "returns" $ do
               solve `shouldReturn` 837799
