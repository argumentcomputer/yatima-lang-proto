module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Spec.IPLD  as IPLD
import           Spec.Parse as Parse
import           Spec.Print as Print

main :: IO ()
main = hspec $ do
  describe "IPLD Serialization" $ IPLD.spec
  describe "Parsing" $ Parse.spec
  describe "Printing" $ Print.spec
