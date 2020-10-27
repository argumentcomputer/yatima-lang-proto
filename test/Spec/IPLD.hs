{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Spec.IPLD where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad.Except

import           Numeric.Natural

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.ByteString.UTF8                 as UTF8
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T

import           Data.IPLD.CID
import           Data.IPLD.DagAST
import           Yatima.IPFS.IPLD
import           Yatima.IPFS.Package
import           Yatima.Term

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text

import           Spec.Instances

prop_separate :: Term -> Bool
prop_separate t = either (const False) id (runExcept $ prop_separate_go t)
  where

prop_separate_go :: Term -> Except IPLDErr Bool
prop_separate_go term = do
  (ast,meta)   <- termToAST "test" term test_index test_cache
  term'        <- astToTerm "test" test_index ast meta
  (ast',meta') <- termToAST "test" term' test_index test_cache
  return $ term == term' && ast == ast' && meta == meta'

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Cid"  $ property $ prop_serial @CID
    it "Meta" $ property $ prop_serial @Meta
    it "DagAST" $ property $ prop_serial @DagAST
    it "Literal" $ property $ prop_serial @Literal
    it "LitType" $ property $ prop_serial @LitType
    it "PrimOp" $ property $ prop_serial @PrimOp
    it "DagDef" $ property $ prop_serial @DagDef
    it "Package" $ property $ prop_serial @Index
    it "Package" $ property $ prop_serial @Package
  describe "Checking metadata separation correctness" $
    it "x == merge (separate x)" $ property prop_separate
