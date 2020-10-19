module Spec.Print where

import qualified Spec.IPLD                            as IPLDSpec
import qualified Spec.Parse                           as ParseSpec
import           Spec.Parse                           (parse)

import           Text.Megaparsec            hiding (State, parse)
import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term

import           Test.Hspec
import           Test.QuickCheck

prop_print_constant :: Constant -> Bool
prop_print_constant t = case ParseSpec.parse pConstant (prettyConstant t) of
  ParseSpec.Good a -> a == t
  _      -> False

prop_print_term :: Term -> Bool
prop_print_term t = case ParseSpec.parse (pExpr False) (prettyTerm t) of
  ParseSpec.Good a -> a == t
  _      -> False

spec :: SpecWith ()
spec = do
  describe "Checking term printing correctness: `x == parse (print x)`" $ do
    it "" $ (withMaxSuccess 10000 $ property $ prop_print_constant)
    it "" $ (withMaxSuccess 10000 $ property $ prop_print_term)
