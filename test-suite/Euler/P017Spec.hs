module Euler.P017Spec
    ( spec ) where

import Euler.P017

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 017" $ do
           it "returns" $ do
               solve `shouldReturn` 21124
