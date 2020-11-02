{-|
Module      : Yatima.DagAST
Description : Encode an Abstract Syntax Tree on the IPLD Directed Acyclic Graph
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
module Data.IPLD.DagAST where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)

import           Data.IPLD.CID

-- | A typed definition embedded in the IPLD DAG
data DagDef = DagDef
  { _termAST  :: CID
  , _typeAST  :: CID
  , _document :: Text
  , _termMeta :: DagMeta
  , _typeMeta :: DagMeta
  } deriving Show

-- | An anonymous spine of a λ-like language
data DagAST
  = Ctor Text [DagAST]
  | Bind DagAST
  | Vari Int
  | Link CID
  | Data BS.ByteString
  deriving (Eq,Show,Ord)

-- | The computationally irrelevant metadata of an AST:
-- Specifically, the names of its binders, the names of its global references
-- and the cids of the DagDefs that correspond to those global references
data DagMeta
  = MCtor [DagMeta]
  | MBind Text DagMeta
  | MLink Text CID
  | MLeaf
  deriving (Eq,Show,Ord)

encodeDagAST :: DagAST -> Encoding
encodeDagAST term = case term of
  Ctor n ts -> encodeListLen 3 
    <> encodeInt 0
    <> encodeString n
    <> encodeListLen (fromIntegral $ length ts)
       <> foldr (\v r -> encodeDagAST v <> r) mempty ts
  Bind t    -> encodeListLen 2 <> encodeInt 1 <> encodeDagAST t
  Vari idx  -> encodeListLen 2 <> encodeInt 2 <> encodeInt idx
  Link cid  -> encodeListLen 2 <> encodeInt 3 <> encodeCid cid
  Data bs   -> encodeListLen 2 <> encodeInt 4 <> encodeBytes bs

decodeDagAST :: Decoder s DagAST
decodeDagAST = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (3,0) -> do
      ctor  <- decodeString
      arity <- decodeListLen
      args  <- replicateM arity decodeDagAST
      return $ Ctor ctor args
    (2,1) -> Bind <$> decodeDagAST
    (2,2) -> Vari <$> decodeInt
    (2,3) -> Link <$> decodeCid
    (2,4) -> Data <$> decodeBytes
    _     -> fail $ concat
      ["invalid DagAST with size: ", show size, " and tag: ", show tag]

instance Serialise DagAST where
  encode = encodeDagAST
  decode = decodeDagAST

encodeDagMeta :: DagMeta -> Encoding
encodeDagMeta term = case term of
  MCtor ts  -> encodeListLen 2 <> encodeInt 0
    <> encodeListLen (fromIntegral $ length ts)
       <> foldr (\v r -> encodeDagMeta v <> r) mempty ts
  MBind n t -> encodeListLen 3 <> encodeInt 1
    <> encodeString n <> encodeDagMeta t
  MLink n c -> encodeListLen 3 <> encodeInt 2
    <> encodeString n <> encodeCid c
  MLeaf     -> encodeListLen 1 <> encodeInt 3

decodeDagMeta :: Decoder s DagMeta
decodeDagMeta = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (2,0) -> do
      arity <- decodeListLen
      args  <- replicateM arity decodeDagMeta
      return $ MCtor args
    (3,1) -> MBind <$> decodeString <*> decodeDagMeta
    (3,2) -> MLink <$> decodeString <*> decodeCid
    (1,3) -> return MLeaf
    _     -> fail $ concat
      ["invalid DagMeta with size: ", show size, " and tag: ", show tag]

instance Serialise DagMeta where
  encode = encodeDagMeta
  decode = decodeDagMeta

encodeDagDef :: DagDef -> Encoding
encodeDagDef (DagDef anonTerm anonType doc termMeta typeMeta) = encodeListLen 6
  <> (encodeString "DagDef")
  <> (encodeCid    anonTerm)
  <> (encodeCid    anonType)
  <> (encodeString doc)
  <> (encodeDagMeta termMeta)
  <> (encodeDagMeta typeMeta)

decodeDagDef :: Decoder s DagDef
decodeDagDef = do
  size     <- decodeListLen
  when (size /= 6) (fail $ "invalid DagDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "DagDef") (fail $ "invalid DagDef tag: " ++ show tag)
  DagDef <$> decodeCid     <*> decodeCid <*> decodeString 
         <*> decodeDagMeta <*> decodeDagMeta

instance Serialise DagDef where
  encode = encodeDagDef
  decode = decodeDagDef

