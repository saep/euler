module Euler.P012Spec
    ( spec ) where

import Euler.P012

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 012" $ do
           it "returns" $ do
               solve `shouldReturn` 76576500
