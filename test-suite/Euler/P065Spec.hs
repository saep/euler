module Euler.P065Spec
    ( spec ) where

import Euler.P065

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem 065" $ do
           it "returns" $ do
               solve `shouldReturn` 272
