{-
Module      : Yatima.IPFS.Package
Description : Defines the Yatima package, which is a map of names to content identifiers
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
module Yatima.IPFS.Package where

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

emptyImports :: Imports
emptyImports = Imports M.empty

emptyIndex :: Index
emptyIndex = Index M.empty M.empty

emptyPackage :: Name -> Package
emptyPackage n = Package n "" (makeCid BSL.empty) emptyImports emptyIndex

newtype Imports = Imports (Map CID Text) deriving (Show, Eq)
newtype Source  = Source  Text           deriving (Show, Eq)

data Index = Index { _byName :: Map Name CID , _byCID  :: Map CID Name }
  deriving (Eq,Show)


mergeIndex :: Index -> Index -> Either (Name,CID,Name,CID) Index
mergeIndex (Index a b) (Index c d) = do
  as <- merge  a c
  bs <- merge' b d
  return $ Index as (M.union b d)

  where
    merge   = M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched f)
    merge'  = M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched g)
    f k x y = if x == y then Right x else Left (k,x,k,y)
    g k x y = if x == y then Right x else Left (x,k,y,k)

data ImportDefs = Full | Some (Set Name) deriving (Eq,Show)

filterIndex :: Index -> ImportDefs -> Index
filterIndex (Index ns cs) ds = case ds of
  Full     -> Index ns cs
  Some ks -> runIdentity $ do
    let ns' = M.restrictKeys ns ks
    let cs' = M.restrictKeys cs (Set.fromList (M.elems ns'))
    return $ Index ns' cs'

encodeIndex :: Index -> Encoding
encodeIndex (Index byName byCID) = encodeListLen 3
  <> (encodeString  ("Index" :: Text))
  <> (encodeMapLen (fromIntegral $ M.size byName) <> encodeByName byName)
  <> (encodeMapLen (fromIntegral $ M.size byCID ) <> encodeByCID  byCID)
  where
    f (k,v) r = encodeString k <> encodeCid v <> r
    g (k,v) r = encodeCid k <> encodeString v <> r
    encodeByName byName = foldr f mempty (sortBy cmp (M.toList byName))
    encodeByCID byCID   = foldr g mempty (sortBy cmp (M.toList byCID))
    cmp (k1,_) (k2,_)   = cborCanonicalOrder (serialise k1) (serialise k2)

cborCanonicalOrder :: BSL.ByteString -> BSL.ByteString -> Ordering
cborCanonicalOrder x y
  | BSL.length x < BSL.length y = LT
  | BSL.length y > BSL.length x = GT
  | otherwise = compare x y

decodeIndex :: Decoder s Index
decodeIndex = do
  size     <- decodeListLen
  when (size /= 3) (fail $ "invalid Index list size: " ++ show size)
  tag    <- decodeString
  when (tag /= "Index") (fail $ "invalid Index tag: " ++ show tag)
  n      <- decodeMapLen
  byName <- M.fromList <$> replicateM n ((,) <$> decodeString <*> decodeCid)
  c      <- decodeMapLen
  byCID  <- M.fromList <$> replicateM c ((,) <$> decodeCid <*> decodeString)
  return $ Index byName byCID

instance Serialise Index where
  encode = encodeIndex
  decode = decodeIndex

encodeImports :: Imports -> Encoding
encodeImports (Imports is) = encodeMapLen (fromIntegral $ M.size is ) <> go is
  where
    f (k,v) r = encodeCid k <> encodeString v <> r
    go is = foldr f mempty (sortBy cmp (M.toList is))
    cmp (k1,_) (k2,_)   = cborCanonicalOrder (serialise k1) (serialise k2)

decodeImports :: Decoder s Imports
decodeImports = do
  n      <- decodeMapLen
  Imports . M.fromList <$> replicateM n ((,) <$> decodeCid <*> decodeString)

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
