module Euler.P050Spec
    ( spec ) where

import Euler.P050

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 050" $ do
           it "returns" $ do
               solve `shouldReturn` 997651
