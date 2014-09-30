module Euler.P028Spec
    ( spec ) where

import Euler.P028

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 028" $ do
           it "returns" $ do
               solve `shouldReturn` 669171001
