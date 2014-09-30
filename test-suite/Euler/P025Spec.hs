module Euler.P025Spec
    ( spec ) where

import Euler.P025

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 025" $ do
           it "returns" $ do
               solve `shouldReturn` 4782
