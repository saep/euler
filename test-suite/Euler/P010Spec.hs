module Euler.P010Spec
    ( spec ) where

import Euler.P010

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 010" $ do
           it "returns" $ do
               solve `shouldReturn` 142913828922
