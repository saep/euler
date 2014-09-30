module Euler.P026Spec
    ( spec ) where

import Euler.P026

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 026" $ do
           it "returns" $ do
               solve `shouldReturn` 983
