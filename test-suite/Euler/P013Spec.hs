module Euler.P013Spec
    ( spec ) where

import Euler.P013

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 013" $ do
           it "returns" $ do
               solve `shouldReturn` 5537376230
