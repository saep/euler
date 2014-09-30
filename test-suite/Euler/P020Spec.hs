module Euler.P020Spec
    ( spec ) where

import Euler.P020

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 020" $ do
           it "returns" $ do
               solve `shouldReturn` 648
