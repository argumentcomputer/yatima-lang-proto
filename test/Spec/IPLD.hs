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

import           Yatima.CID
import           Yatima.DagAST
import           Yatima.IPLD
import           Yatima.Package
import           Yatima.Term
import           Yatima.Uses

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text

instance Arbitrary CID where
  arbitrary = makeCID <$> (arbitrary :: Gen ByteString)

deriving instance Bounded Uses

instance Arbitrary Uses where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AnonAST where
  arbitrary = oneof
    [ Vari <$> arbitrary
    , Link <$> arbitrary
    , Bind <$> arbitrary
    , Data <$> arbitrary
    , do
        n <- (\n -> (n * 2) `div` 3) <$> getSize
        i <- choose (0,n)
        c <- name_gen
        ts <- resize n $ vector i
        return $ Ctor c ts
    ]

instance Arbitrary Meta where
  arbitrary = Meta <$> arbitrary

instance Arbitrary AnonDef where
  arbitrary = AnonDef <$> arbitrary <*> arbitrary

deriving instance Eq AnonDef

instance Arbitrary DagDef where
  arbitrary = DagDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Eq DagDef

instance Arbitrary Imports where
  arbitrary = Imports <$> arbitrary

instance Arbitrary Index where
  arbitrary = Index <$> arbitrary <*> arbitrary

instance Arbitrary Package where
  arbitrary =
    Package <$> name_gen <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_serial :: (Eq a, Serialise a) => a -> Bool
prop_serial x = let s = serialise x in 
  case (deserialiseOrFail s) of
    Left _   -> False
    Right x' -> x == x' && (serialise x' == s)

test_defs :: (Index,Cache)
test_defs =
  let trm = Lam "A" (Lam "x" (Var "x"))
      typ = All "A" Many Typ (All "x" Many (Var "A") (Var "A"))
      def = Def "" trm typ
      Right (cid, cache) = runExcept (insertDef "id" def emptyIndex (Cache M.empty))
   in (Index (M.singleton "id" cid) (M.singleton cid "id"), cache)

test_index = fst test_defs
test_cache = snd test_defs

name_gen :: Gen Text
name_gen = do
  a <- elements $ ['A'..'Z'] ++ ['a'..'z']
  n <- choose (0,100) :: Gen Int
  return $ T.cons a (T.pack $ show n)

literal_gen :: Gen Literal
literal_gen = oneof
  [ return VWorld
  , return TWorld
  , VNatural <$> arbitrarySizedNatural
  , VBitString <$> arbitrary
  , do 
    len <- choose (1,64) :: Gen Int
    val <- BS.pack <$> vectorOf len arbitrary
    return $ VBitVector (fromIntegral len*8) val
  , VString . UTF8.fromString <$> arbitrary
  , VChar <$> arbitrary
  , return TNatural
  , return TBitString
  , TBitVector <$> arbitrarySizedNatural
  , return TString
  , return TChar
  ]

instance Arbitrary Literal where
  arbitrary = literal_gen

instance Arbitrary PrimOp where
  arbitrary = arbitraryBoundedEnum

term_gen :: [Name] -> Gen Term
term_gen ctx = frequency
  [ (100, Var <$> elements ctx)
  , (100, Ref <$> elements (M.keys (_byName test_index)))
  , (100, return Typ)
  , (100, Lit <$> arbitrary)
  , (100, Opr <$> arbitrary)
  , (50, (name_gen >>= \n -> Lam n <$> term_gen (n:ctx)))
  , (50, (name_gen >>= \n -> Slf n <$> term_gen (n:ctx)))
  , (50, App <$> term_gen ctx <*> term_gen ctx)
  , (50, Ann <$> term_gen ctx <*> term_gen ctx)
  , (33, (name_gen >>= \n -> All n <$> arbitrary <*> term_gen ctx <*> term_gen (n:ctx)))
  , (33, (name_gen >>= \n -> 
            Let n <$> arbitrary <*> term_gen ctx <*> term_gen (n:ctx)
                  <*> term_gen (n:ctx)))
  ]

instance Arbitrary Term where
  arbitrary = term_gen ["test"]

prop_separate :: Term -> Bool
prop_separate t = either (const False) id (runExcept $ prop_separate_go t)
  where

prop_separate_go :: Term -> Except IPLDErr Bool
prop_separate_go term = do
  (ast,meta)   <- termToAST "test" term test_index test_cache
  term'        <- astToTerm "test" test_index ast meta
  (ast',meta') <- termToAST "test" term' test_index test_cache
  return $ term == term' && ast == ast' && meta == meta'

spec :: SpecWith ()
spec = do
  describe "Checking serialisation correctness: `x == deserialise (serialise x)`" $ do
    it "Cid"  $ property $ prop_serial @CID
    it "Meta" $ property $ prop_serial @Meta
    it "Anon" $ property $ prop_serial @AnonAST
    it "Literal" $ property $ prop_serial @Literal
    it "PrimOp" $ property $ prop_serial @PrimOp
    it "ASTDef" $ property $ prop_serial @AnonDef
    it "DagDef" $ property $ prop_serial @DagDef
    it "Package" $ property $ prop_serial @Index
    it "Package" $ property $ prop_serial @Package
  describe "Checking metadata separation correctness" $
    it "x == merge (separate x)" $ property prop_separate
