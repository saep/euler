module Euler.P043Spec
    ( spec ) where

import Euler.P043

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 043" $ do
           it "returns" $ do
               solve `shouldReturn` 16695334890
