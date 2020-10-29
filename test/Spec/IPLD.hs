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
import           Yatima.IPLD
import           Yatima.Package
import           Yatima.Term

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text

import           Spec.Instances

prop_separate_term :: Term -> Bool
prop_separate_term d = either (const False) (all id) (runExcept $ prop_separate_term_go d)

prop_separate_term_go :: Term -> Except DagError [Bool]
prop_separate_term_go term = do
  let (termAST,termMeta) = (termToAST term, termToMeta term)
  term' <- dagToTerm [] termAST termMeta
  let (termAST',termMeta') = (termToAST term', termToMeta term')
  return [term == term', termAST == termAST', termMeta == termMeta']

prop_separate_def :: Def -> Bool
prop_separate_def d = either (const False) (all id) (runExcept $ prop_separate_def_go d)

prop_separate_def_go :: Def -> Except DagError [Bool]
prop_separate_def_go def = do
  let (termAST,termMeta,typeAST,typeMeta) = defToDag def
  def' <- dagToDef (_doc def) "test" (termAST,termMeta) (typeAST,typeMeta)
  let (termAST',termMeta',typeAST',typeMeta') = defToDag def'
  return [def == def', termAST == termAST', termMeta == termMeta'
         , typeAST == typeAST', typeMeta == typeMeta'
         ]

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Cid"      $ withMaxSuccess 1000 $ property $ prop_serial @CID
    it "DagMeta"  $ withMaxSuccess 1000 $ property $ prop_serial @DagMeta
    it "DagAST"   $ withMaxSuccess 1000 $ property $ prop_serial @DagAST
    it "Literal"  $ withMaxSuccess 1000 $ property $ prop_serial @Literal
    it "LitType"  $ withMaxSuccess 1000 $ property $ prop_serial @LitType
    it "PrimOp"   $ withMaxSuccess 1000 $ property $ prop_serial @PrimOp
    it "DagDef"   $ withMaxSuccess 1000 $ property $ prop_serial @DagDef
    it "Package"  $ withMaxSuccess 1000 $ property $ prop_serial @Index
    it "Package"  $ withMaxSuccess 1000 $ property $ prop_serial @Package
  describe "Checking metadata separation correctness" $ do
    it "x == merge (separate x)" $ withMaxSuccess 1000 $ property prop_separate_term
    it "x == merge (separate x)" $ withMaxSuccess 1000 $ property prop_separate_def
