{-|
Module      : Yatima.DagAST
Description : Encode an Abstract Syntax Tree on the IPLD Directed Acyclic Graph
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
module Yatima.DagAST where

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

import           Yatima.CID
import           Yatima.Term

-- | A definition embedded in the IPLD DAG
data DagDef = DagDef
  { _anonDef  :: CID
  , _document :: Text
  , _termMeta :: Meta
  , _typeMeta :: Meta
  } deriving Show

-- | An anonymous definition of a term and a type
data AnonDef = AnonDef { _anonTerm :: CID , _anonType :: CID } deriving Show

-- | An anonymous spine of a λ-like language
data AnonAST
  = Ctor Name [AnonAST]
  | Bind AnonAST
  | Vari Int
  | Link CID
  | Data BS.ByteString
  deriving (Eq,Show,Ord)

-- | The computationally irrelevant metadata of an AST
data Meta = Meta { _entries :: IntMap (Either Name CID) } deriving (Show,Eq)

encodeAnonAST :: AnonAST -> Encoding
encodeAnonAST term = case term of
  Ctor n ts -> encodeListLen 3 
    <> encodeInt 0
    <> encodeString n
    <> encodeListLen (fromIntegral $ length ts)
       <> foldr (\v r -> encodeAnonAST v <> r) mempty ts
  Bind t    -> encodeListLen 2 <> encodeInt 1 <> encodeAnonAST t
  Vari idx  -> encodeListLen 2 <> encodeInt 2 <> encodeInt idx
  Link cid  -> encodeListLen 2 <> encodeInt 3 <> encodeCID cid
  Data bs   -> encodeListLen 2 <> encodeInt 4 <> encodeBytes bs

decodeAnonAST :: Decoder s AnonAST
decodeAnonAST = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (3,0) -> do
      ctor  <- decodeString
      arity <- decodeListLen
      args  <- replicateM arity decodeAnonAST
      return $ Ctor ctor args
    (2,1) -> Bind <$> decodeAnonAST
    (2,2) -> Vari <$> decodeInt
    (2,3) -> Link <$> decodeCID
    (2,4) -> Data <$> decodeBytes
    _     -> fail $ concat
      ["invalid AnonAST with size: ", show size, " and tag: ", show tag]

instance Serialise AnonAST where
  encode = encodeAnonAST
  decode = decodeAnonAST

encodeMeta :: Meta -> Encoding
encodeMeta (Meta m) = encodeMapLen (fromIntegral (IM.size m))
  <> IM.foldrWithKey go mempty m
  where
    go = (\k v r -> encodeString (T.pack $ show k) <> encodeEntry v <> r)
    encodeEntry (Left n)  = encodeInt 0 <> encodeString n
    encodeEntry (Right c) = encodeInt 1 <> encodeCID c

decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  Meta . IM.fromList <$> replicateM size decodeEntry
  where
    decodeEntry = do
      keyString <- decodeString
      let  key = (read (T.unpack keyString) :: Int)
      tag <- decodeInt
      case tag of
        0 -> do
          n <- decodeString
          return (key,Left n)
        1 -> do
          c <- decodeCID
          return (key,Right c)
        _ -> fail "invalid Meta map entry"

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeAnonDef :: AnonDef -> Encoding
encodeAnonDef (AnonDef term typ_) = encodeListLen 3
  <> (encodeString "AnonDef")
  <> (encodeCID term)
  <> (encodeCID typ_)

decodeAnonDef :: Decoder s AnonDef
decodeAnonDef = do
  size     <- decodeListLen
  when (size /= 3) (fail $ "invalid AnonDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "AnonDef") (fail $ "invalid AnonDef tag: " ++ show tag)
  AnonDef <$> decodeCID <*> decodeCID

instance Serialise AnonDef where
  encode = encodeAnonDef
  decode = decodeAnonDef

encodeDagDef :: DagDef -> Encoding
encodeDagDef (DagDef anonDef doc termMeta typeMeta) = encodeListLen 5
  <> (encodeString "DagDef")
  <> (encodeCID    anonDef)
  <> (encodeString doc)
  <> (encodeMeta   termMeta)
  <> (encodeMeta   typeMeta)

decodeDagDef :: Decoder s DagDef
decodeDagDef = do
  size     <- decodeListLen
  when (size /= 5) (fail $ "invalid DagDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "DagDef") (fail $ "invalid DagDef tag: " ++ show tag)
  DagDef <$> decodeCID <*> decodeString <*> decodeMeta <*> decodeMeta

instance Serialise DagDef where
  encode = encodeDagDef
  decode = decodeDagDef

