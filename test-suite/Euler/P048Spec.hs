module Euler.P048Spec
    ( spec ) where

import Euler.P048

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 048" $ do
           it "returns" $ do
               solve `shouldReturn` 9110846700
