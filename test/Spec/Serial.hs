{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Spec.Serial where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad.Except

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Language.Yatima.CID
import           Language.Yatima.Defs
import           Language.Yatima.Term
import           Language.Yatima.Uses

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text

instance Arbitrary CID where
  arbitrary = makeCID <$> (arbitrary :: Gen ByteString)

deriving instance Bounded Uses

instance Arbitrary Uses where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Anon where
  arbitrary = frequency
    [ (100,VarA <$> arbitrary)
    , (100,RefA <$> arbitrary)
    , (100, return TypA)
    , (50, LamA <$> arbitrary <*> arbitrary)
    , (50, AppA <$> arbitrary <*> arbitrary)
    , (33, AllA <$> arbitrary <*> arbitrary <*> arbitrary)
    , (33, LetA <$> (arbitrary :: Gen Uses) <*> (arbitrary :: Gen Anon)
           <*> (arbitrary :: Gen Anon) <*> (arbitrary :: Gen Anon))
    ]

instance Arbitrary Meta where
  arbitrary = Meta <$> arbitrary <*> arbitrary

instance Arbitrary AnonDef where
  arbitrary = AnonDef <$> arbitrary <*> arbitrary

deriving instance Eq AnonDef

instance Arbitrary MetaDef where
  arbitrary = MetaDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Eq MetaDef

instance Arbitrary Package where
  arbitrary = Package <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_serial :: (Eq a, Serialise a) => a -> Bool
prop_serial x = let s = serialise x in 
  case (deserialiseOrFail s) of
    Left _   -> False
    Right x' -> x == x' && (serialise x' == s)


test_defs :: (Index,Cache)
test_defs =
  let trm = Lam "A" (Just(Many,Typ)) (Lam "x" (Just(Many,Var "A")) (Var "x"))
      typ = All "" "A" Many Typ (All "" "x" Many (Var "A") (Var "A"))
      def = Def "id" "" trm typ
      Right (cid, cache) = runExcept (insertDef def M.empty M.empty)
   in (M.singleton "id" cid, cache)

test_index = fst test_defs
test_cache = snd test_defs

name_gen :: Gen Text
name_gen = T.pack <$> (listOf $ elements nameChar)
  where
    nameChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

ann_gen :: [Name] -> Gen (Maybe (Uses, Term))
ann_gen ctx = oneof
  [ return Nothing
  , fmap Just $ (,) <$> arbitrary <*> (term_gen ctx)
  ]

term_gen :: [Name] -> Gen Term
term_gen ctx = frequency
  [ (100,Var <$> elements ctx)
  , (100,Ref <$> elements (M.keys test_index))
  , (100, return Typ)
  , (50, (name_gen >>= \n -> Lam n <$> ann_gen ctx <*> term_gen (n:ctx)))
  , (50, App <$> term_gen ctx <*> term_gen ctx)
  , (33, (name_gen >>= \s -> name_gen >>= \n -> 
            All s n <$> arbitrary <*> term_gen ctx <*> term_gen (n:s:ctx)))
  , (33, (name_gen >>= \n -> 
            Let n <$> arbitrary <*> term_gen ctx <*> term_gen (n:ctx)
                  <*> term_gen (n:ctx)))
  ]

instance Arbitrary Term where
  arbitrary = term_gen ["test"]

prop_separate :: Term -> Bool
prop_separate t = either (const False) id (runExcept $ go t)
  where
    go :: Term -> Except DerefErr Bool
    go t = do
      (a,m) <- separateMeta "test" t test_index test_cache
      t'    <- mergeMeta a m
      (a',m') <- separateMeta "test" t' test_index test_cache
      return $ t == t' && a' == a && m == m'

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Cid"  $ property $ prop_serial @CID
    it "Meta" $ property $ prop_serial @Meta
    it "Anon" $ property $ prop_serial @Anon
    it "AnonDef" $ property $ prop_serial @AnonDef
    it "MetaDef" $ property $ prop_serial @MetaDef
  describe "Checking metadata separation correctness" $
    it "x == merge (separate x)" $ property prop_separate
