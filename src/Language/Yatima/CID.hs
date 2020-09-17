{-# LANGUAGE DataKinds   #-}
module Language.Yatima.CID 
  ( CID(..)
  , encodeCID
  , decodeCID
  , cidToBase
  , cidToBytes
  , cidFromBase
  , hashLazyWith
  , mkCborCIDv1
  , makeCID
  , cidFromText
  , cidToText
  , printCIDBase32
  ) where

import qualified Data.ByteString.Multibase  as MB

import qualified Data.ByteString.Lazy       as BSL
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as Build
import qualified Data.ByteString.Multibase  as MB
import qualified Data.ByteString.BaseN      as BaseN
import qualified Data.ByteString.Base16     as BS16
import qualified Data.ByteString.Base16     as BS16

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Data.IPLD.CID              (CID, cidFromText, cidToText)
import qualified Data.IPLD.CID              as CID
import qualified Data.Multihash             as MH
import qualified Crypto.Hash as C

encodeCID :: CID -> Encoding
encodeCID cid =
  let cid_bytestring = BSL.toStrict $ Build.toLazyByteString $ CID.buildCid cid
      cid_atBaseID   = BaseN.encodeAtBase BaseN.BaseIdentity cid_bytestring
      cid_multibase  = MB.fromMultibase $ MB.encode cid_atBaseID
   in encodeTag 42 <> encodeBytes cid_multibase

cidToBase :: (MB.ToCode b) => BaseN.Base b -> CID -> ByteString
cidToBase base cid =
  let cid_bytestring = BSL.toStrict $ Build.toLazyByteString $ CID.buildCid cid
      cid_atBase     = BaseN.encodeAtBase base cid_bytestring
      cid_multibase  = MB.fromMultibase $ MB.encode cid_atBase
    in cid_multibase

cidToBytes :: CID -> ByteString
cidToBytes cid = BSL.toStrict $ Build.toLazyByteString $ CID.buildCid cid

cidFromBase :: ByteString -> Either String CID
cidFromBase bs = do
  mb  <- MB.decode bs
  cid <- CID.decodeCid mb
  return cid

decodeCID :: Decoder s CID
decodeCID = do
  tag   <- decodeTag
  if tag /= 42
  then fail "CBOR Link tag not equal to 42"
  else do
    base <- decodeBytes
    case cidFromBase base of
      Left str -> fail $ "CID decoding error: " ++ str
      Right cid -> return cid

instance Serialise CID where
  encode = encodeCID
  decode = decodeCID

hashLazyWith :: C.HashAlgorithm alg => alg -> BSL.ByteString -> C.Digest alg
hashLazyWith _ bs = C.hashlazy bs

mkCborCIDv1 :: (MH.Multihashable alg, Serialise a) => alg -> a -> CID
mkCborCIDv1 alg a = CID.newCidV1 CID.DagCbor (hashLazyWith alg (serialise a))

makeCID :: Serialise a => a -> CID
makeCID a = mkCborCIDv1 C.Blake2b_256 a

printCIDBase32 :: CID -> ByteString
printCIDBase32 c = cidToBase BaseN.Base32 c

