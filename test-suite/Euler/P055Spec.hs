module Euler.P055Spec
    ( spec ) where

import Euler.P055

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 055" $ do
           it "returns" $ do
               solve `shouldReturn` 249
