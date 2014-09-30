module Euler.P040Spec
    ( spec ) where

import Euler.P040

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 040" $ do
           it "returns" $ do
               solve `shouldReturn` 210
