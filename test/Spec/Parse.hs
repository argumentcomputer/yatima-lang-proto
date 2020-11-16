{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Parse where

import Control.Monad.Identity
import Data.IPLD.DagPackage
import Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Spec.Instances as Instances
import Test.Hspec
import Text.Megaparsec hiding (ParseError, State, parse)
import Yatima.Parse.Parser hiding (Parser)
import qualified Yatima.Parse.Parser as Parse
import Yatima.Parse.Term
import Yatima.Term

data Result a
  = Good a
  | Bad Int (ErrorFancy (ParseError ()))
  | Ugly (ParseErrorBundle Text (ParseError ()))
  deriving (Eq, Show)

type Parser a = Parse.Parser () Identity a

testParseEnv :: Parse.ParseEnv
testParseEnv = Parse.ParseEnv ["test"] (indexEntries Instances.test_index)

parse :: Parser a -> Text -> Result a
parse p txt = case runIdentity $ Parse.parseM p testParseEnv "" txt of
  Left e -> case bundleErrors e of
    FancyError pos es NE.:| _ -> case Set.toList es of
      [x] -> Bad pos x
      _ -> Ugly e
    _ -> Ugly e
  Right x -> Good x

mkBad :: Int -> ParseError () -> Result a
mkBad pos e = Bad pos (ErrorCustom e)

parseIt :: (Eq a, Show a) => Parser a -> Text -> Result a -> SpecWith (Arg Expectation)
parseIt p txt res = it (T.unpack txt) $ parse p txt `shouldBe` res

parseDescribe :: (Eq a, Show a) => Parser a -> Text -> [(Text, Result a)] -> SpecWith ()
parseDescribe p desc xs = describe (T.unpack desc) $ do
  traverse (\(t, r) -> parseIt p t r) xs
  return ()

spec :: SpecWith ()
spec = do
  parseDescribe
    (pName False)
    "Names:"
    [ ("a", Good "a"),
      ("a1", Good "a1"),
      ("a'", Good "a'"),
      ("a_", Good "a_"),
      ("a", Good "a"),
      ("a1", Good "a1"),
      ("a'", Good "a'"),
      ("a_", Good "a_")
    ]
  parseDescribe
    (pName False)
    "Name Errors:"
    [ ("1", mkBad 1 $ LeadingDigit "1"),
      ("'a", mkBad 2 $ ReservedLeadingChar '\'' "'a"),
      ("let", mkBad 3 $ ReservedKeyword "let"),
      ("A.Foo", Good "A.Foo")
    ]
  --parseIt (pName True >> eof) "A.Foo"  Ugly

  parseDescribe
    pLam
    "Lambda"
    [ ("λ x => x", Good $ _Lam "x" $ _Var "x" 0),
      ("λ x => λ y => x", Good $ _Lam "x" $ _Lam "y" $ _Var "x" 1),
      ("λ x y => x", Good $ _Lam "x" $ _Lam "y" $ _Var "x" 1),
      ("\\ x => x", Good $ _Lam "x" $ _Var "x" 0),
      ("\\ x => \\ y => x", Good $ _Lam "x" $ _Lam "y" $ _Var "x" 1),
      ("\\ x y => x", Good $ _Lam "x" $ _Lam "y" $ _Var "x" 1)
    ]

  --parseDescribe
  --  (pBinder False)
  --  "Binder, name mandatory"
  --  [ ("(A:Type)", Good [(NoLoc, "A", Many, _Typ)]),
  --    ("(A B C :Type)", Good [(NoLoc, "A", Many, _Typ), (3, "B", Many, _Typ), (5, "C", Many, _Typ)])
  --  ]

  --
  --  parseDescribe
  --    (pBinder True)
  --    "Binder, name optional"
  --    [ ("Type", Good [(0, "", Many, Typ 0)]),
  --      ("Type", Good [(0, "", Many, Typ 0)])
  --    ]
  --
  parseDescribe
    pAll
    "Binder, in forall, with repetition "
    [ ( "∀ (A: Type) (a: A) (b: A) -> A",
        Good $
          _All "A" Many (_Typ) (_All "a" Many (_Var "A" 0) (_All "b" Many (_Var "A" 1) (_Var "A" 2)))
      ),
      ( "∀ (A: Type) (a b: A) -> A",
        Good $
          _All "A" Many (_Typ) (_All "a" Many (_Var "A" 0) (_All "b" Many (_Var "A" 1) (_Var "A" 2)))
      ),
      ( "∀ (A B: Type) (a b: A) (c d: B) -> A",
        Good $
          _All "A" Many (_Typ) (_All "B" Many (_Typ) (_All "a" Many (_Var "A" 1) (_All "b" Many (_Var "A" 2) (_All "c" Many (_Var "B" 2) (_All "d" Many (_Var "B" 3) (_Var "A" 5))))))
      )
    ]

  parseDescribe
    pAll
    "Forall"
    [ ( "∀ (x: Type) -> Type",
        Good $ _All "x" Many (_Typ) (_Typ)
      ),
      ( "∀ (x: Type) -> x",
        Good $ _All "x" Many (_Typ) $ _Var "x" 0
      ),
      ( "∀ (x: Type) (y: Type) -> x",
        Good $ _All "x" Many (_Typ) $ _All "y" Many (_Typ) $ _Var "x" 1
      ),
      ( "∀ (A: Type) (x: A) -> x",
        Good $ _All "A" Many (_Typ) $ _All "x" Many (_Var "A" 0) $ _Var "x" 0
      ),
      ( "∀ (x: Type) -> x",
        Good $ _All "x" Many (_Typ) $ _Var "x" 0
      ),
      ( "∀ (0 x: Type) -> x",
        Good $ _All "x" None (_Typ) $ _Var "x" 0
      ),
      ( "∀ (& x: Type) -> x",
        Good $ _All "x" Affi (_Typ) $ _Var "x" 0
      ),
      ( "∀ (1 x: Type) -> x",
        Good $ _All "x" Once (_Typ) $ _Var "x" 0
      )
    ]

  parseDescribe
    pTyp
    "Typ"
    [ ("Type", Good $ _Typ)
    ]

  parseDescribe
    (pDecl True False)
    "Declarations"
    [ ("foo: Type = Type", Good ("foo", _Typ, _Typ)),
      ( "foo (A:Type) (B:Type) (x:A) (y:B) : A = x",
        Good $
          ( "foo",
            _Lam "A" (_Lam "B" (_Lam "x" (_Lam "y" (_Var "x" 1)))),
            _All
              "A"
              Many
              (_Typ)
              ( _All
                  "B"
                  Many
                  (_Typ)
                  ( _All
                      "x"
                      Many
                      (_Var "A" 1)
                      (_All "y" Many (_Var "B" 1) (_Var "A" 3))
                  )
              )
          )
      )
    ]

  parseDescribe
    pLet
    "Let"
    [ ( "let any: Type = Type; any",
        Good $ _Let False "any" Many (_Typ) (_Typ) $ _Var "any" 0
      ),
      ( "let any (x:Type) (y:Type): Type = Type; any",
        Good $
          _Let
            False
            "any"
            Many
            (_All "x" Many (_Typ) (_All "y" Many (_Typ) (_Typ)))
            (_Lam "x" (_Lam "y" (_Typ)))
            (_Var "any" 0)
      )
    ]
