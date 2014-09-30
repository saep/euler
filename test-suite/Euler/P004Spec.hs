module Euler.P004Spec
    ( spec ) where

import Euler.P004

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 004" $ do
           it "returns" $ do
               solve `shouldReturn` 906609
