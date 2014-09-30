module Euler.P046Spec
    ( spec ) where

import Euler.P046

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 046" $ do
           it "returns" $ do
               solve `shouldReturn` 5777
