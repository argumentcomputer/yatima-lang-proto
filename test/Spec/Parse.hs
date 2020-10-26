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

import           Yatima.Term
import qualified Yatima.Parse as Parse
import           Yatima.Parse hiding (Parser)

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
    , ("'a", mkBad 2 $ ReservedLeadingChar '\'' "'a")
    , ("let", mkBad 3 $ ReservedKeyword "let")
    , ("A/Foo", Good "A/Foo")
    ]
    --parseIt (pName True >> eof) "A/Foo"  Ugly

  parseDescribe pLam "Lambda"
    [ ("λ x => x", Good $ Lam "x" $ Var "x")
    , ("λ x => λ y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "λ x y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "\\ x => x", Good $ Lam "x" $ Var "x")
    , ( "\\ x => \\ y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    , ( "\\ x y => x", Good $ Lam "x" $ Lam "y" $ Var "x")
    ]

  parseDescribe (pBinder False) "Binder, name mandatory"
    [("(A:Type)", Good [("A",Many,Typ)])
    ,("(A B C :Type)", Good [("A",Many,Typ), ("B",Many,Typ), ("C",Many,Typ)])
    ]
  parseDescribe (pBinder True) "Binder, name optional"
    [("Type", Good [("",Many,Typ)])
    ,("Type", Good [("",Many,Typ)])
    ]


  parseDescribe pAll "Forall"
    [ ( "∀ (x: Type) -> Type", Good $ All "x" Many Typ Typ)
    , ( "∀ (x: Type) -> x", Good $ All "x" Many Typ $ Var "x")
    , ( "∀ (x: Type) (y: Type) -> x", Good $ All "x" Many Typ $ All "y" Many Typ $ Var "x")
    , ( "∀ (A: Type) (x: A) -> x", Good $ All "A" Many Typ $ All "x" Many (Var "A") $ Var "x")
    , ( "∀ (x: Type) -> x", Good $ All  "x" Many Typ $ Var "x")
    , ( "∀ (0 x: Type) -> x", Good $ All "x" None Typ $ Var "x")
    , ( "∀ (& x: Type) -> x", Good $ All "x" Affi Typ $ Var "x")
    , ( "∀ (1 x: Type) -> x", Good $ All "x" Once Typ $ Var "x")
    ]

  parseDescribe pTyp "Typ"
    [("Type", Good Typ)
    ]

  parseDescribe (pDecl False) "Declarations"
    [ ("foo: Type = Type", Good ("foo", Typ, Typ))
    , ("foo (A:Type) (B:Type) (x:A) (y:B) : A = x", Good $ 
        ( "foo"
        , Lam "A" (Lam "B" (Lam "x" (Lam "y" (Var "x"))))
        , All "A" Many Typ (All "B" Many Typ (All "x" Many (Var "A")
          (All "y" Many (Var "B") (Var "A"))))
        )
      )
    ]

  parseDescribe pLet "Let"
    [ ("let any: Type = Type; any", Good $ Let False "any" Many Typ Typ $ Var "any")
    , ("let any (x:Type) (y:Type): Type = Type; any", Good $ 
          Let False "any" Many (All "x" Many Typ (All "y" Many Typ Typ))
            (Lam "x" (Lam "y" Typ)) (Var "any")
      )
    ]

