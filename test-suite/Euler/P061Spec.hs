module Euler.P061Spec
    ( spec ) where

import Euler.P061

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 061" $ do
           it "returns" $ do
               solve `shouldReturn` 28684
