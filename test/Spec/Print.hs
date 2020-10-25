{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec.Print where

import qualified Spec.IPLD                            as IPLDSpec
import qualified Spec.Parse                           as ParseSpec
import           Spec.Parse                           (parse)

import           Text.Megaparsec            hiding (State, parse)

import           Yatima.Parse
import           Yatima.Print
import           Yatima.Term
import           Yatima.QuasiQuoter

import           Test.Hspec
import           Test.QuickCheck

prop_print_literal :: Literal -> Bool
prop_print_literal t = case ParseSpec.parse pLiteral (prettyLiteral t) of
  ParseSpec.Good a -> a == t
  _      -> False

prop_print_litType :: LitType -> Bool
prop_print_litType t = case ParseSpec.parse pLitType (prettyLitType t) of
  ParseSpec.Good a -> a == t
  _      -> False

prop_print_term :: Term -> Bool
prop_print_term t = case ParseSpec.parse (pExpr False) (prettyTerm t) of
  ParseSpec.Good a -> a == t
  _      -> False

spec :: SpecWith ()
spec = do
  describe "Checking App printing test cases" $ do
    it "" $ (prettyTerm [yatima| λ x => x |])      `shouldBe` "λ x => x"
    it "" $ (prettyTerm [yatima| λ x => x x|])     `shouldBe` "λ x => x x"
    it "" $ (prettyTerm [yatima| λ x => (x x)|])   `shouldBe` "λ x => x x"
    it "" $ (prettyTerm [yatima| λ x => x x x |])  `shouldBe` "λ x => x x x"
    it "" $ (prettyTerm [yatima| λ x => (x x) x |])  `shouldBe` "λ x => x x x"
    it "" $ (prettyTerm [yatima| λ x => x (x x) |])  `shouldBe` "λ x => x (x x)"
    it "" $ (prettyTerm [yatima| λ x => (x x x) |])  `shouldBe` "λ x => x x x"
    it "" $ (prettyTerm [yatima| λ x => x x x x |])  `shouldBe` "λ x => x x x x"
    it "" $ (prettyTerm [yatima| λ x => (x x) x x |])  `shouldBe` "λ x => x x x x"
    it "" $ (prettyTerm [yatima| λ x => x (x x) x |])  `shouldBe` "λ x => x (x x) x"
    it "" $ (prettyTerm [yatima| λ x => x x (x x)|])  `shouldBe` "λ x => x x (x x)"
    it "" $ (prettyTerm [yatima| λ x => (x x x) x |])  `shouldBe` "λ x => x x x x"
    it "" $ (prettyTerm [yatima| λ x => x (x x x) |])  `shouldBe` "λ x => x (x x x)"
    it "" $ (prettyTerm [yatima| λ x => (x (x x)) x |])  `shouldBe` "λ x => x (x x) x"
  describe "Checking term printing correctness: `x == parse (print x)`" $ do
    it "" $ (withMaxSuccess 1000  $ property $ prop_print_litType)
    it "" $ (withMaxSuccess 10000 $ property $ prop_print_literal)
    it "" $ (withMaxSuccess 10000 $ property $ prop_print_term)
