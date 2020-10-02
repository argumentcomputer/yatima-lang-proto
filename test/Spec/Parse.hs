{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Parse where

import           Control.Monad.Identity


import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.List.NonEmpty         (NonEmpty)
import           Data.List.NonEmpty         as NE

import           Language.Yatima.Term
import           Language.Yatima.Uses
import qualified Language.Yatima.Parse as Parse
import           Language.Yatima.Parse hiding (Parser)

import qualified Spec.IPLD                            as IPLDSpec

import           Test.Hspec

import           Text.Megaparsec            hiding (State, parse)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.RawString.QQ


data Result a
  = Good a
  | Bad Int (ErrorFancy (ParseErr ()))
  | Ugly (ParseErrorBundle Text (ParseErr ()))
  deriving (Eq, Show)

type Parser a = Parse.Parser () Identity a

testParseEnv :: Parse.ParseEnv
testParseEnv = Parse.ParseEnv (Set.fromList ["test"]) (Set.fromList ["id"])

parse :: Parser a -> Text -> Result a
parse p txt = case runIdentity $ Parse.parseM p testParseEnv "" txt of
  Left e -> case bundleErrors e of
    FancyError pos es NE.:| _ -> case Set.toList es of
      [x] -> Bad pos x
      _   -> Ugly e
    _                         -> Ugly e
  Right x -> Good x

mkBad :: Int -> ParseErr () -> Result a
mkBad pos e = Bad pos (ErrorCustom e)

parseIt :: (Eq a, Show a) => Parser a -> Text -> Result a -> SpecWith (Arg Expectation)
parseIt p txt res = it (T.unpack txt) $ parse p txt `shouldBe` res

parseDescribe :: (Eq a, Show a) => Parser a -> Text -> [(Text,Result a)] -> SpecWith ()
parseDescribe p desc xs = describe (T.unpack desc) $ do
    traverse (\(t,r) -> parseIt p t r) xs
    return ()

spec :: SpecWith ()
spec = do
  parseDescribe (pName False) "Names:"
    [ ("a" , Good "a")
    , ("a1", Good "a1")
    , ("a'", Good "a'")
    , ("a_", Good "a_")
    , ("a" , Good "a")
    , ("a1", Good "a1")
    , ("a'", Good "a'")
    , ("a_", Good "a_")
    ]
  parseDescribe (pName False) "Name Errors:"
    [ ("1", mkBad 1 $ LeadingDigit "1")
    , ("'a", mkBad 2 $ LeadingApostrophe "'a")
    , ("let", mkBad 3 $ ReservedKeyword "let")
    , ("A/Foo", Good "A/Foo")
    ]
    --parseIt (pName True >> eof) "A/Foo"  Ugly

  parseDescribe pLam "Lambda"
    [ ("λ x => x", Good $ Lam "x" $ Var "x")
    , ("λ x => λ y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "λ x y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "lam x => x", Good $ Lam "x" $ Var "x")
    , ( "lambda x => x", Good $ Lam "x" $ Var "x")
    , ( "lam x => lam y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "lambda x => lambda y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "lam x y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "lambda x y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    ]

  parseDescribe (pBinder False) "Binder, name mandatory"
    [("(A:*)", Good [("A",Many,Any)])
    ,("(A B C :*)", Good [("A",Many,Any), ("B",Many,Any), ("C",Many,Any)])
    ]
  parseDescribe (pBinder True) "Binder, name optional"
    [("*", Good [("",Many,Any)])
    ,("*", Good [("",Many,Any)])
    ]

  parseDescribe pHol "Hole"
    [("?foo", Good $ Hol "foo")
    ]

  parseDescribe pHol "Hole Errors"
    [("?'a", mkBad 3 $ LeadingApostrophe "'a")
    ]

  parseDescribe pAll "Forall"
    [ ( "∀ (x: *) -> *", Good $ All "" "x" Many Any Any)
    , ( "∀ (x: *) -> x", Good $ All "" "x" Many Any $ Var "x")
    , ( "∀ (x: *) (y: *) -> x", Good $ All "" "x" Many Any $ All "" "y" Many Any $ Var "x")
    , ( "∀ (A: *) (x: A) -> x" , Good $ All "" "A" Many Any $ All "" "x" Many (Var "A") $ Var "x")
    , ( "@self ∀ (x: *) -> x", Good $ All "self" "x" Many Any $ Var "x")
    , ( "@self ∀ (x: *) -> self", Good $ All "self" "x" Many Any $ Var "self")
    , ( "@self ∀ (0 x: *) -> x", Good $ All "self" "x" None Any $ Var "x")
    , ( "@self ∀ (& x: *) -> x", Good $ All "self" "x" Affi Any $ Var "x")
    , ( "@self ∀ (1 x: *) -> x", Good $ All "self" "x" Once Any $ Var "x")
    , ( "all (x: *) -> *", Good $ All "" "x" Many Any Any)
    , ( "all (x: *) -> x", Good $ All "" "x" Many Any $ Var "x")
    , ( "all (x: *) (y: *) -> x", Good $ All "" "x" Many Any $ All "" "y" Many Any $ Var "x")
    , ( "all (A: *) (x: A) -> x", Good $ All "" "A" Many Any $ All "" "x" Many (Var "A") $ Var "x")
    , ( "@self all (x: *) -> x", Good $ All "self" "x" Many Any $ Var "x")
    , ( "@self all (x: *) -> self", Good $ All "self" "x" Many Any $ Var "self")
    , ( "@self all (0 x: *) -> x", Good $ All "self" "x" None Any $ Var "x")
    , ( "@self all (& x: *) -> x", Good $ All "self" "x" Affi Any $ Var "x")
    , ( "@self all (1 x: *) -> x", Good $ All "self" "x" Once Any $ Var "x")
    , ( "forall (x: *) -> *", Good $ All "" "x" Many Any Any)
    , ( "forall (x: *) -> x", Good $ All "" "x" Many Any $ Var "x")
    , ( "forall (x: *) (y: *) -> x", Good $ All "" "x" Many Any $ All "" "y" Many Any $ Var "x")
    , ( "forall (A: *) (x: A) -> x", Good $ All "" "A" Many Any $ All "" "x" Many (Var "A") $ Var "x")
    , ( "@self forall (x: *) -> x",  Good $ All "self" "x" Many Any $ Var "x")
    , ( "@self forall (x: *) -> self", Good $ All "self" "x" Many Any $ Var "self")
    , ( "@self forall (0 x: *) -> x", Good $ All "self" "x" None Any $ Var "x")
    , ( "@self forall (& x: *) -> x", Good $ All "self" "x" Affi Any $ Var "x")
    , ( "@self forall (1 x: *) -> x", Good $ All "self" "x" Once Any $ Var "x")
    ]

  parseDescribe pAny "Any"
    [("*", Good Any)
    ]

  parseDescribe (pDecl False) "Declarations"
    [ ("foo: * = *", Good ("foo", Any, Any))
    , ("foo (A:*) (B:*) (x:A) (y:B) : A = x", Good $ 
        ( "foo"
        , Lam "A" (Lam "B" (Lam "x" (Lam "y" (Var "x"))))
        , All "" "A" Many Any (All "" "B" Many Any (All "" "x" Many (Var "A")
          (All "" "y" Many (Var "B") (Var "A"))))
        )
      )
    ]

  parseDescribe pLet "Let"
    [ ("let any: * = *; any", Good $ Let "any" Many Any Any $ Var "any")
    , ("let any (x:*) (y:*): * = *; any", Good $ 
          Let "any" Many (All "" "x" Many Any (All "" "y" Many Any Any))
            (Lam "x" (Lam "y" Any)) (Var "any")
      )
    ]

