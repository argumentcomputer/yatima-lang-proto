{-
Module      : Yatima.Package
Description : Defines the Yatima package, which is a map of names to content identifiers
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
module Yatima.Package where

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
import           Yatima.Term

data Package = Package
  { _title   :: Name
  , _descrip :: Text
  , _source  :: CID
  , _imports :: Imports
  , _index   :: Index
  } deriving (Show, Eq)

emptyPackage :: Name -> Package
emptyPackage n = Package n "" (makeCid BSL.empty) emptyImports emptyIndex

newtype Imports = Imports [(CID,Text)]         deriving (Show, Eq)
newtype Source  = Source  Text                 deriving (Show, Eq)
newtype Index   = Index   (Map Name (CID,CID)) deriving (Show, Eq)

emptyImports :: Imports
emptyImports = Imports []

emptyIndex :: Index
emptyIndex = Index M.empty

indexEntries :: Index -> Map Name (CID,CID)
indexEntries (Index ns) = ns

mergeIndex :: Index -> Index -> Either (Name,CID,CID) Index
mergeIndex (Index a) (Index b) = do
  Index <$> merge a b
  where
    merge   = M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched f)
    f k x y = if x == y then Right x else Left (k,fst x, fst y)

data ImportDefs = Full | Some (Set Name) deriving (Eq,Show)

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

encodeSource :: Source -> Encoding
encodeSource (Source txt) = encodeString txt

decodeSource :: Decoder s Source
decodeSource = Source <$> decodeString

instance Serialise Source where
  encode = encodeSource
  decode = decodeSource

encodePackage :: Package -> Encoding
encodePackage package = encodeListLen 6
  <> (encodeString  ("Package" :: Text))
  <> (encodeString  (_title   package))
  <> (encodeString  (_descrip package))
  <> (encodeCid     (_source package))
  <> (encodeImports (_imports package))
  <> (encodeIndex   (_index   package))

decodePackage :: Decoder s Package
decodePackage = do
  size     <- decodeListLen
  when (size /= 6) (fail $ "invalid Package list size: " ++ show size)
  tag <- decodeString
  when (tag /= "Package") (fail $ "invalid Package tag: " ++ show tag)
  Package <$> decodeString <*> decodeString <*> decodeCid
  <*> decodeImports <*> decodeIndex

instance Serialise Package where
  encode = encodePackage
  decode = decodePackage
