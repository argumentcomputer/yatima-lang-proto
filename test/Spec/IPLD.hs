{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.IPLD where

import Codec.Serialise
import Control.Monad.Except
import Data.Aeson hiding (decode, encode)
import qualified Data.Aeson as Aeson
import Data.IPLD.CID
import Data.IPLD.DagAST
import Data.IPLD.DagJSON
import Data.IPLD.DagPackage
import Spec.Instances ()
import Test.Hspec
import Test.QuickCheck
import Yatima.IPLD
import Yatima.Term

prop_serial :: (Eq a, Serialise a) => a -> Bool
prop_serial x =
  let s = serialise x
   in case (deserialiseOrFail s) of
        Left _ -> False
        Right x' -> x == x' && (serialise x' == s)

prop_json_encode :: forall a. (Eq a, Serialise a) => a -> Bool
prop_json_encode x = maybe False (const True) $ do
  let eitherToMaybe = either (const Nothing) Just
  let s = serialise x
  v <- eitherToMaybe (deserialiseOrFail @DagJSON s)
  unless (serialise v == s) (fail "")
  v' <- eitherToMaybe (eitherDecode' @DagJSON (Aeson.encode v))
  unless (v == v' && serialise v' == s) (fail "")
  x' <- eitherToMaybe (deserialiseOrFail @a (serialise v'))
  unless (x == x' && serialise x' == s) (fail "")
  return ()

prop_separate_term :: Term -> Bool
prop_separate_term d = either (const False) (all id) (runExcept $ prop_separate_term_go d)

prop_separate_term_go :: Term -> Except DagError [Bool]
prop_separate_term_go term = do
  let (termAST, termMeta) = (termToAST term, termToMeta term)
  term' <- dagToTerm [] termAST termMeta
  let (termAST', termMeta') = (termToAST term', termToMeta term')
  return [term == term', termAST == termAST', termMeta == termMeta']

prop_separate_def :: Def -> Bool
prop_separate_def d = either (const False) (all id) (runExcept $ prop_separate_def_go d)

prop_separate_def_go :: Def -> Except DagError [Bool]
prop_separate_def_go def = do
  let (termAST, termMeta, typeAST, typeMeta) = defToDag def
  def' <- dagToDef "test" (_doc def) "test" (termAST, termMeta) (typeAST, typeMeta)
  let (termAST', termMeta', typeAST', typeMeta') = defToDag def'
  return
    [ def == def',
      termAST == termAST',
      termMeta == termMeta',
      typeAST == typeAST',
      typeMeta == typeMeta'
    ]

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Uses" $ property $ prop_serial @Uses
    it "Cid" $ property $ prop_serial @CID
    it "DagMeta" $ property $ prop_serial @DagMeta
    it "DagAST" $ property $ prop_serial @DagAST
    it "Literal" $ property $ prop_serial @Literal
    it "LitType" $ property $ prop_serial @LitType
    it "PrimOp" $ property $ prop_serial @PrimOp
    it "DagDef" $ property $ prop_serial @DagDef
    it "Index" $ property $ prop_serial @Index
    it "DagSource" $ property $ prop_serial @DagSource
    it "DagPackage" $ property $ prop_serial @DagPackage
    it "DagYatima" $ property $ prop_serial @DagYatima
  describe "Checking JSON serialisation correctness" $ do
    it "Uses" $ property $ prop_json_encode @Uses
    it "Cid" $ property $ prop_json_encode @CID
    it "DagMeta" $ property $ prop_json_encode @DagMeta
    it "DagAST" $ property $ prop_json_encode @DagAST
    it "Literal" $ property $ prop_json_encode @Literal
    it "LitType" $ property $ prop_json_encode @LitType
    it "PrimOp" $ property $ prop_json_encode @PrimOp
    it "DagDef" $ property $ prop_json_encode @DagDef
    it "Index" $ property $ prop_json_encode @Index
    it "DagSource" $ property $ prop_json_encode @DagSource
    it "DagPackage" $ property $ prop_json_encode @DagPackage
    it "DagYatima" $ property $ prop_json_encode @DagYatima
  describe "Checking metadata separation correctness" $ do
    it "x == merge (separate x)" $ withMaxSuccess 1000 $ property prop_separate_term
    it "x == merge (separate x)" $ withMaxSuccess 1000 $ property prop_separate_def
