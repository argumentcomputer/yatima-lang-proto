{-
Module      : Data.IPLD.DagDagPackage
Description : Defines a package of DagDefs
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
module Data.IPLD.DagPackage where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad
import           Control.Monad.Identity

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Data.Map.Merge.Strict      as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.List                  (sortBy)

import           Data.IPLD.CID
import           Data.IPLD.DagAST

data DagPackage = DagPackage
  { _packageTitle :: Text
  , _description  :: Text
  , _sourceFile   :: CID
  , _imports      :: Imports
  , _index        :: Index
  } deriving (Show, Eq)

emptyDagPackage :: Text -> DagPackage
emptyDagPackage n = DagPackage n "" (makeCid BSL.empty) emptyImports emptyIndex

newtype Imports = Imports [(CID,Text)]         deriving (Show, Eq)
newtype Index   = Index   (Map Text (CID,CID)) deriving (Show, Eq)

data DagSource  = DagSource
  { _srcTitle :: Text
  , _srcTxt :: Text
  } deriving (Show, Eq)

emptyImports :: Imports
emptyImports = Imports []

emptyIndex :: Index
emptyIndex = Index M.empty

indexEntries :: Index -> Map Text (CID,CID)
indexEntries (Index ns) = ns

mergeIndex :: Index -> Index -> Either (Text,CID,CID) Index
mergeIndex (Index a) (Index b) = do
  Index <$> merge a b
  where
    merge   = M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched f)
    f k x y = if x == y then Right x else Left (k,fst x, fst y)

data ImportDefs = Full | Some (Set Text) deriving (Eq,Show)

filterIndex :: Index -> ImportDefs -> Index
filterIndex (Index ns) ds = case ds of
  Full    -> Index ns
  Some ks -> Index (M.restrictKeys ns ks)

encodeIndex :: Index -> Encoding
encodeIndex (Index ds) = encodeListLen 2
  <> (encodeString  ("Index" :: Text))
  <> (encodeMapLen (fromIntegral $ M.size ds) <> encodeEntries ds)
  where
    encodeEntries ds = foldr f mempty (sortBy cmp (M.toList ds))
    f (k,(d,t)) r = encodeString k <> encodeListLen 2 <> encodeCid d <> encodeCid t <> r
    cmp (k1,_) (k2,_)   = cborCanonicalOrder (serialise k1) (serialise k2)

cborCanonicalOrder :: BSL.ByteString -> BSL.ByteString -> Ordering
cborCanonicalOrder x y
  | BSL.length x < BSL.length y = LT
  | BSL.length y > BSL.length x = GT
  | otherwise = compare x y

decodeIndex :: Decoder s Index
decodeIndex = do
  size     <- decodeListLen
  when (size /= 2) (fail $ "invalid Index list size: " ++ show size)
  tag    <- decodeString
  when (tag /= "Index") (fail $ "invalid Index tag: " ++ show tag)
  n  <- decodeMapLen
  let decodeCids = decodeListLen >> (,) <$> decodeCid <*> decodeCid
  ds <- M.fromList <$> replicateM n ((,) <$> decodeString <*> decodeCids)
  return $ Index ds

instance Serialise Index where
  encode = encodeIndex
  decode = decodeIndex

encodeImports :: Imports -> Encoding
encodeImports (Imports is) = encodeListLen (fromIntegral $ length is) <> go is
  where
    f (k,v) r = encodeListLen 2 <> encodeCid k <> encodeString v <> r
    go is = foldr f mempty is

decodeImports :: Decoder s Imports
decodeImports = do
  n      <- decodeListLen
  let decodeEntry = decodeListLen >> ((,) <$> decodeCid <*> decodeString)
  Imports <$> replicateM n decodeEntry

instance Serialise Imports where
  encode = encodeImports
  decode = decodeImports

encodeDagSource :: DagSource -> Encoding
encodeDagSource (DagSource title txt) = 
  encodeListLen 3
  <> encodeString "DagSource"
  <> encodeString title
  <> encodeString txt

decodeDagSource :: Decoder s DagSource
decodeDagSource = do
  size <- decodeListLen
  tag  <- decodeString
  when (tag /= "DagSource") (fail $ "invalid DagSource tag: " ++ show tag)
  when (size /= 3) (fail $ "invalid DagSource list size: " ++ show size)
  DagSource <$> decodeString <*> decodeString

instance Serialise DagSource where
  encode = encodeDagSource
  decode = decodeDagSource

encodeDagPackage :: DagPackage -> Encoding
encodeDagPackage package = encodeListLen 6
  <> (encodeString  "DagPackage")
  <> (encodeString  (_packageTitle package))
  <> (encodeString  (_description package))
  <> (encodeCid     (_sourceFile  package))
  <> (encodeImports (_imports     package))
  <> (encodeIndex   (_index       package))

decodeDagPackage :: Decoder s DagPackage
decodeDagPackage = do
  size     <- decodeListLen
  tag  <- decodeString
  when (tag /= "DagPackage") (fail $ "invalid DagPackage tag: " ++ show tag)
  when (size /= 6) (fail $ "invalid DagPackage list size: " ++ show size)
  DagPackage <$> decodeString <*> decodeString <*> decodeCid
          <*> decodeImports <*> decodeIndex

instance Serialise DagPackage where
  encode = encodeDagPackage
  decode = decodeDagPackage

packageImportCids :: DagPackage -> Set CID
packageImportCids (DagPackage _ _ _ (Imports ms) _) = Set.fromList $ fst <$> ms

packageIndexCids :: DagPackage -> Set CID
packageIndexCids (DagPackage _ _ _ _ (Index ns)) =
  let elems = M.elems ns in
  Set.union (Set.fromList $ fst <$> elems) (Set.fromList $ snd <$> elems)
