module Euler.P039Spec
    ( spec ) where

import Euler.P039

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 039" $ do
           it "returns" $ do
               solve `shouldReturn` 840
