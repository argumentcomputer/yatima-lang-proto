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

prop_print_test :: Term -> ParseSpec.Result Term
prop_print_test t = ParseSpec.parse (pExpr False) (prettyTerm t)

prop_print :: Term -> Bool
prop_print t = case prop_print_test t of
  ParseSpec.Good a -> a == t
  _      -> False

spec :: SpecWith ()
spec = do
  describe "Checking term printing correctness: `x == parse (print x)`" $ do
    it "" $ (withMaxSuccess 10000 $ property $ prop_print)
