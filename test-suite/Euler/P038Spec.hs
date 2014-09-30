module Euler.P038Spec
    ( spec ) where

import Euler.P038

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 038" $ do
           it "returns" $ do
               solve `shouldReturn` 932718654
