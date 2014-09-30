module Euler.P023Spec
    ( spec ) where

import Euler.P023

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 023" $ do
           it "returns" $ do
               solve `shouldReturn` 4179871
