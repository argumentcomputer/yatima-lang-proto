-- |
-- Module      : Yatima.DagAST
-- Description : Encode an Abstract Syntax Tree on the IPLD Directed Acyclic Graph
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Data.IPLD.DagAST where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.IPLD.Cid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T hiding (find)
import qualified Data.Text.Encoding as T

-- | A typed definition embedded in the IPLD DAG
data DagDef = DagDef
  { _title :: Text,
    _termAST :: Cid,
    _typeAST :: DagAST,
    _document :: Text,
    _termMeta :: DagMeta,
    _typeMeta :: DagMeta
  }
  deriving (Eq, Show)

-- | An anonymous spine of a λ-like language
data DagAST
  = Ctor Text [DagAST]
  | Bind DagAST
  | Vari Int
  | Link Cid
  | Data BS.ByteString
  deriving (Eq, Show, Ord)

-- | The computationally irrelevant metadata of an AST:
-- Specifically, the names of its binders, the names of its global references
-- and the cids of the DagDefs that correspond to those global references
data DagMeta
  = MCtor [DagMeta]
  | MBind Text DagMeta
  | MLink Text Cid
  | MLeaf
  deriving (Eq, Show, Ord)

encodeData :: BS.ByteString -> Encoding
encodeData bs = encodeString (encodeBase64 bs)

decodeData :: Decoder s BS.ByteString
decodeData = do
  dat <- T.encodeUtf8 <$> decodeString
  case (decodeBase64 dat, isValidBase64 dat) of
    (Right bs, True) -> return bs
    (Left e, _) ->
      fail ("invalid Data, expected a valid base64 string: " ++ T.unpack e)

encodeDagAST :: DagAST -> Encoding
encodeDagAST term = case term of
  Ctor n ts ->
    encodeListLen 3
      <> encodeString "Ctor"
      <> encodeString n
      <> encodeListLen (fromIntegral $ length ts)
      <> foldr (\v r -> encodeDagAST v <> r) mempty ts
  Bind t -> encodeListLen 2 <> encodeString "Bind" <> encodeDagAST t
  Vari idx -> encodeListLen 2 <> encodeString "Vari" <> encodeInt idx
  Link cid -> encodeListLen 2 <> encodeString "Link" <> encodeCid cid
  Data bs -> encodeListLen 2 <> encodeString "Data" <> encodeData bs

decodeDagAST :: Decoder s DagAST
decodeDagAST = do
  size <- decodeListLen
  tag <- decodeString
  case (size, tag) of
    (3, "Ctor") -> do
      ctor <- decodeString
      arity <- decodeListLen
      args <- replicateM arity decodeDagAST
      return $ Ctor ctor args
    (2, "Bind") -> Bind <$> decodeDagAST
    (2, "Vari") -> Vari <$> decodeInt
    (2, "Link") -> Link <$> decodeCid
    (2, "Data") -> Data <$> decodeData
    _ ->
      fail $
        concat
          ["invalid DagAST with size: ", show size, " and tag: ", show tag]

instance Serialise DagAST where
  encode = encodeDagAST
  decode = decodeDagAST

encodeDagMeta :: DagMeta -> Encoding
encodeDagMeta term = case term of
  MCtor ts ->
    encodeListLen 2 <> encodeString "MCtor"
      <> encodeListLen (fromIntegral $ length ts)
      <> foldr (\v r -> encodeDagMeta v <> r) mempty ts
  MBind n t ->
    encodeListLen 3 <> encodeString "MBind"
      <> encodeString n
      <> encodeDagMeta t
  MLink n c ->
    encodeListLen 3 <> encodeString "MLink"
      <> encodeString n
      <> encodeCid c
  MLeaf -> encodeListLen 1 <> encodeString "MLeaf"

decodeDagMeta :: Decoder s DagMeta
decodeDagMeta = do
  size <- decodeListLen
  tag <- decodeString
  case (size, tag) of
    (2, "MCtor") -> do
      arity <- decodeListLen
      args <- replicateM arity decodeDagMeta
      return $ MCtor args
    (3, "MBind") -> MBind <$> decodeString <*> decodeDagMeta
    (3, "MLink") -> MLink <$> decodeString <*> decodeCid
    (1, "MLeaf") -> return MLeaf
    _ ->
      fail $
        concat
          ["invalid DagMeta with size: ", show size, " and tag: ", show tag]

instance Serialise DagMeta where
  encode = encodeDagMeta
  decode = decodeDagMeta

encodeDagDef :: DagDef -> Encoding
encodeDagDef (DagDef title anonTerm anonType doc termMeta typeMeta) =
  encodeListLen 7
    <> (encodeString "DagDef")
    <> (encodeString title)
    <> (encodeCid anonTerm)
    <> (encodeDagAST anonType)
    <> (encodeString doc)
    <> (encodeDagMeta termMeta)
    <> (encodeDagMeta typeMeta)

decodeDagDef :: Decoder s DagDef
decodeDagDef = do
  size <- decodeListLen
  tag <- decodeString
  when (tag /= "DagDef") (fail $ "invalid DagDef tag: " ++ show tag)
  when (size /= 7) (fail $ "invalid DagDef list size: " ++ show size)
  DagDef <$> decodeString <*> decodeCid <*> decodeDagAST <*> decodeString
    <*> decodeDagMeta
    <*> decodeDagMeta

instance Serialise DagDef where
  encode = encodeDagDef
  decode = decodeDagDef

dagMetaCids :: DagMeta -> Set Cid
dagMetaCids t = go Set.empty t
  where
    go :: Set Cid -> DagMeta -> Set Cid
    go cids t = case t of
      MCtor ds -> Set.unions $ go cids <$> ds
      MBind _ d -> go cids d
      MLink _ c -> Set.insert c cids
      MLeaf -> cids

dagASTCids :: DagAST -> Set Cid
dagASTCids t = go Set.empty t
  where
    go :: Set Cid -> DagAST -> Set Cid
    go cids t = case t of
      Ctor _ ds -> Set.unions $ go cids <$> ds
      Bind d -> go cids d
      Link c -> Set.insert c cids
      Data _ -> cids
      Vari _ -> cids
