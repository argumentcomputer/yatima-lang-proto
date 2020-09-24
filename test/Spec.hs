module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Spec.IPLD as IPLD

main :: IO ()
main = hspec $ do
  describe "Yatima IPLD Serialization" $ IPLD.spec
