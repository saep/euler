module Euler.P045Spec
    ( spec ) where

import Euler.P045

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 045" $ do
           it "returns" $ do
               solve `shouldReturn` 1533776805
