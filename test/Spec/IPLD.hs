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
import           Yatima.Package
import           Yatima.Term

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text

import           Spec.Instances

fromRight (Right x) = x
fromRight (Left e) = error "fromRight"

term = App (Let False "l19" Once (Var "test" 0) Typ (Lam "m16" (App refId refId))) (Slf "A29" (Var "test" 1))

refId =
  let d = cidFromText "bafy2bzaceb7tzcelrtfuo4zl375mtm7dqwmvv7a4amlpziwbm7k3hr4bp3lfc"
      t = cidFromText "bafy2bzaceagf5dbfewoq632a5x5mjhhv3ojftx2sdh3lc73cneocoe7chzsks"
   in Ref "id" (fromRight d) (fromRight t)

prop_separate :: Term -> Bool
prop_separate t = either (const False) (all id) (runExcept $ prop_separate_go t)

prop_separate_go :: Term -> Except IPLDErr [Bool]
prop_separate_go term = do
  let ast  = termToAST  term
  let meta = termToMeta term
  term'        <- astToTerm "test" test_index ast meta
  let ast'  = termToAST term'
  let meta' = termToMeta term'
  return [term == term', ast == ast', meta == meta']

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Cid"      $ withMaxSuccess 1000 $ property $ prop_serial @CID
    it "Meta"     $ withMaxSuccess 1000 $ property $ prop_serial @Meta
    it "DagAST"   $ withMaxSuccess 1000 $ property $ prop_serial @DagAST
    it "Literal"  $ withMaxSuccess 1000 $ property $ prop_serial @Literal
    it "LitType"  $ withMaxSuccess 1000 $ property $ prop_serial @LitType
    it "PrimOp"   $ withMaxSuccess 1000 $ property $ prop_serial @PrimOp
    it "DagDef"   $ withMaxSuccess 1000 $ property $ prop_serial @DagDef
    it "Package"  $ withMaxSuccess 1000 $ property $ prop_serial @Index
    it "Package"  $ withMaxSuccess 1000 $ property $ prop_serial @Package
  describe "Checking metadata separation correctness" $
    it "x == merge (separate x)" $ (withMaxSuccess 1000 $ property prop_separate)
