module Euler.P031Spec
    ( spec ) where

import Euler.P031

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 031" $ do
           it "returns" $ do
               solve `shouldReturn` 73682
