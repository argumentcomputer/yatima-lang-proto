module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Spec.Serial as Serial

main :: IO ()
main = hspec $ do
  describe "Yatima Serialization" $ Serial.spec
