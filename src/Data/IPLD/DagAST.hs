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
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM

import           Data.IPLD.CID

-- | A typed definition embedded in the IPLD DAG
data DagDef = DagDef
  { _anonTerm :: CID
  , _anonType :: CID
  , _document :: Text
  , _termMeta :: Meta
  , _typeMeta :: Meta
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
data Meta = Meta { _entries :: IntMap (Text, Maybe CID) } deriving (Show,Eq)

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

encodeMeta :: Meta -> Encoding
encodeMeta (Meta m) = encodeMapLen (fromIntegral (IM.size m))
  <> IM.foldrWithKey go mempty m
  where
    go = (\k v r -> encodeString (T.pack $ show k) <> encodeEntry v <> r)
    encodeEntry (n, Nothing) = encodeListLen 1 <> encodeString n
    encodeEntry (n, Just c)  = encodeListLen 2 <> encodeString n <> encodeCid c

decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  Meta . IM.fromList <$> replicateM size decodeEntry
  where
    decodeEntry = do
      keyString <- decodeString
      let  key = (read (T.unpack keyString) :: Int)
      size <- decodeListLen
      case size of
        1 -> do
          n <- decodeString
          return (key,(n,Nothing))
        2 -> do
          n <- decodeString
          c <- decodeCid
          return (key,(n,Just c))
        _ -> fail "invalid Meta map entry"

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeDagDef :: DagDef -> Encoding
encodeDagDef (DagDef anonTerm anonType doc termMeta typeMeta) = encodeListLen 6
  <> (encodeString "DagDef")
  <> (encodeCid    anonTerm)
  <> (encodeCid    anonType)
  <> (encodeString doc)
  <> (encodeMeta   termMeta)
  <> (encodeMeta   typeMeta)

decodeDagDef :: Decoder s DagDef
decodeDagDef = do
  size     <- decodeListLen
  when (size /= 6) (fail $ "invalid DagDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "DagDef") (fail $ "invalid DagDef tag: " ++ show tag)
  DagDef <$> decodeCid <*> decodeCid <*> decodeString <*> decodeMeta <*> decodeMeta

instance Serialise DagDef where
  encode = encodeDagDef
  decode = decodeDagDef

