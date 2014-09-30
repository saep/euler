module Euler.P044Spec
    ( spec ) where

import Euler.P044

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 044" $ do
           it "returns" $ do
               solve `shouldReturn` 5482660
