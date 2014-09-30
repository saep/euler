module Euler.P049Spec
    ( spec ) where

import Euler.P049

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 049" $ do
           it "returns" $ do
               solve `shouldReturn` 296962999629
