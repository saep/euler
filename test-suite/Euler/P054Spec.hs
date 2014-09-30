module Euler.P054Spec
    ( spec ) where

import Euler.P054

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 054" $ do
           it "returns" $ do
               solve `shouldReturn` 376
