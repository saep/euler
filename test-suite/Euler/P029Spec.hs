module Euler.P029Spec
    ( spec ) where

import Euler.P029

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 029" $ do
           it "returns" $ do
               solve `shouldReturn` 9183
