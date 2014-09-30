module Euler.P035Spec
    ( spec ) where

import Euler.P035

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 035" $ do
           it "returns" $ do
               solve `shouldReturn` 55
