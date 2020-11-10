{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Instances where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IPLD.Cid
import Data.IPLD.DagAST
import Data.IPLD.DagPackage
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Gen ()
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Text ()
import Yatima.IPLD
import Yatima.Term

instance Arbitrary Cid where
  arbitrary = makeCid <$> (arbitrary :: Gen ByteString)

deriving instance Bounded Uses

instance Arbitrary Uses where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DagAST where
  arbitrary =
    oneof
      [ Vari <$> arbitrary,
        Link <$> arbitrary,
        Bind <$> arbitrary,
        Data <$> arbitrary,
        do
          n <- (\n -> (n * 3) `div` 5) <$> getSize
          i <- choose (0, n)
          c <- name_gen
          ts <- resize n $ vector i
          return $ Ctor c ts
      ]

instance Arbitrary DagMeta where
  arbitrary =
    oneof
      [ return MLeaf,
        MLink <$> name_gen <*> arbitrary,
        MBind <$> name_gen <*> arbitrary,
        do
          n <- (\n -> (n * 3) `div` 5) <$> getSize
          i <- choose (0, n)
          ts <- resize n $ vector i
          return $ MCtor ts
      ]

instance Arbitrary DagDef where
  arbitrary =
    DagDef "test"
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Imports where
  arbitrary = Imports <$> arbitrary

instance Arbitrary Index where
  arbitrary = Index <$> arbitrary

instance Arbitrary DagPackage where
  arbitrary =
    DagPackage <$> name_gen <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

test_index :: Index
test_index =
  let trm = Lam "A" (Lam "x" (Var "x" 0))
      typ = All "A" Many Typ (All "x" Many (Var "A" 0) (Var "A" 1))
      def = Def "id" "" trm typ
      cid = makeCid $ defToDagDef def
      cid' = makeCid $ termToAST trm
   in Index (M.singleton "id" (cid, cid'))

name_gen :: Gen Text
name_gen = do
  a <- elements $ ['A' .. 'Z'] ++ ['a' .. 'z']
  n <- choose (0, 100) :: Gen Int
  return $ T.cons a (T.pack $ show n)

literal_gen :: Gen Literal
literal_gen =
  oneof
    [ return VWorld,
      VNatural <$> arbitrarySizedNatural,
      do
        len <- choose (1, 64) :: Gen Int
        val <- BS.pack <$> vectorOf len arbitrary
        return $ VBitVector (fromIntegral len * 8) val,
      VString <$> arbitrary,
      VChar <$> arbitrary,
      VI64 <$> arbitrary,
      VI32 <$> arbitrary,
      VF64 <$> arbitrary,
      VF32 <$> arbitrary,
      return VException
    ]

literalType_gen :: Gen LitType
literalType_gen =
  oneof
    [ return TWorld,
      return TNatural,
      return TBitVector,
      return TString,
      return TChar,
      return TI64,
      return TI32,
      return TF64,
      return TF64,
      return TException
    ]

instance Arbitrary Literal where
  arbitrary = literal_gen

instance Arbitrary LitType where
  arbitrary = literalType_gen

instance Arbitrary PrimOp where
  arbitrary = arbitraryBoundedEnum

var_gen :: [Name] -> Gen Term
var_gen ctx = do
  n <- elements ctx
  return $ Var n (fromJust $ findByName n ctx)

ref_gen :: Gen Term
ref_gen = do
  n <- elements (M.keys $ indexEntries $ test_index)
  let (x, y) = (indexEntries test_index) M.! n
  return $ Ref n x y

term_gen :: [Name] -> Gen Term
term_gen ctx =
  frequency
    [ if ctx == [] then (0, return Typ) else (100, var_gen ctx),
      (100, ref_gen),
      (100, return Typ),
      (100, Lit <$> arbitrary),
      (100, LTy <$> arbitrary),
      (100, Opr <$> arbitrary),
      (50, (name_gen >>= \n -> Lam n <$> term_gen (n : ctx))),
      (50, (name_gen >>= \n -> Slf n <$> term_gen (n : ctx))),
      (33, App <$> term_gen ctx <*> term_gen ctx),
      (33, Ann <$> term_gen ctx <*> term_gen ctx),
      ( 25,
        ( name_gen >>= \n ->
            All n <$> arbitrary <*> term_gen ctx <*> term_gen (n : ctx)
        )
      ),
      ( 25,
        ( do
            nam <- name_gen
            rec <- arbitrary
            Let rec nam <$> arbitrary <*> term_gen ctx
              <*> term_gen (if rec then (nam : ctx) else ctx)
              <*> term_gen (nam : ctx)
        )
      )
    ]

instance Arbitrary Term where
  arbitrary = term_gen []

instance Arbitrary DagSource where
  arbitrary = DagSource "Test" <$> arbitrary

instance Arbitrary Def where
  arbitrary = Def "test" <$> arbitrary <*> term_gen ["test"] <*> term_gen []

instance Arbitrary DagYatima where
  arbitrary =
    oneof
      [ YatimaPackage <$> arbitrary,
        YatimaDef <$> arbitrary,
        YatimaAST <$> arbitrary,
        YatimaSource <$> arbitrary
      ]
