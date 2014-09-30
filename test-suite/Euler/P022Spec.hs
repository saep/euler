module Euler.P022Spec
    ( spec ) where

import Euler.P022

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 022" $ do
           it "returns" $ do
               solve `shouldReturn` 871198282
