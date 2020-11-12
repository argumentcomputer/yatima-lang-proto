{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Data.IPLD.Cid
-- Description : This module implements the Content-IDentifiers (Cids) used in
-- IPFS, IPLD and Filecoin.
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- \"Cid is a format for referencing content in distributed information systems,
-- like IPFS. It leverages content addressing, cryptographic hashing, and
-- self-describing formats. It is the core identifier used by IPFS and IPLD.\"
-- <https://github.com/ipld/cid Content IDentifiers>
-- k
-- This module modifies work by [Monadic
-- GmbH](https://github.com/monadic-xyz/ipfs/blob/master/ipld-cid/src/Data/IPLD/Cid.hs)
-- which is licensed under BSD3 terms included with this package in the
-- @licenses/2018_Monadic_GmbH@ file.
module Data.IPLD.Cid
  ( Version (..),
    Codec (..),
    Cid (..),
    newCidV0,
    newCidV1,
    buildCid,
    getCid,
    cidFromText,
    cidFromText',
    cidFromMultihashBytes,
    cidFromMultibaseBytes,
    cidToText,
    cidToMultibaseBytes,
    codecToCode,
    codecFromCode,
    decodeCid,
    encodeCid,
    makeCid,
    makeCidFromBytes,
  )
where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as C
import Data.Bifunctor (bimap)
import qualified Data.Binary.Get as Binary
import Data.Binary.VarInt (buildVarInt, getVarInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Multibase as Multibase
import Data.Data
import Data.Hashable (Hashable)
import Data.Multihash (Multihash, Multihashable)
import qualified Data.Multihash as Multihash
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | Specification version.
data Version = V0 | V1 deriving (Eq, Show, Ord, Enum, Bounded, Generic, Data)

instance Hashable Version

instance NFData Version

-- | The content type or format of the data being addressed, specified as a
-- <https://github.com/multiformats/multicodec multicodec>.
--
-- Note that we do not currently have a full multicodec implementation, as it is
-- overly complicated for our purposes. We also only support 'Codec's on an
-- as-needed basis. Future versions may utilise a separate library.
data Codec
  = Raw
  | DagProtobuf
  | DagCbor
  | GitRaw
  deriving (Eq, Show, Ord, Generic, Data)

instance Hashable Codec

instance NFData Codec

-- | A Content IDentifier.
--
-- * 'V0' 'Cid's are merely SHA256 hashes, base58-encoded using the bitcoin
-- alphabet. The 'Codec' is implicitly 'DagProtobuf'.
-- * 'V1' 'Cid's may use any 'Multihash', and any of the supported 'Codec's.
data Cid = Cid
  { cidVersion :: Version,
    cidCodec :: Codec,
    cidHash :: Multihash
  }
  deriving (Eq, Ord, Generic, Data)

instance Hashable Cid

instance NFData Cid

instance Show Cid where
  show = Text.unpack . cidToText

instance Read Cid where
  readsPrec _ =
    either (const []) (\cid -> [(cid, "")]) . cidFromText . Text.pack

-- | Create a 'V0' 'Cid'.
newCidV0 :: Digest SHA256 -> Cid
newCidV0 dig =
  Cid
    { cidVersion = V0,
      cidCodec = DagProtobuf,
      cidHash = Multihash.fromDigest dig
    }

-- | Create a 'V1' 'Cid'.
newCidV1 :: Multihashable a => Codec -> Digest a -> Cid
newCidV1 codec dig =
  Cid
    { cidVersion = V1,
      cidCodec = codec,
      cidHash = Multihash.fromDigest dig
    }

-- | Serialise a 'Cid'.
buildCid :: Cid -> Builder
buildCid Cid {..} = case cidVersion of
  V0 -> Builder.byteString (Multihash.encodedBytes cidHash)
  V1 ->
    buildVarInt (fromEnum cidVersion)
      <> buildVarInt (codecToCode cidCodec)
      <> Builder.byteString (Multihash.encodedBytes cidHash)

-- | Decode a 'Cid' from a strict 'ByteString'.
--
-- @
--    cidFromMultihashBytes . buildCid ≡ Right
-- @
cidFromMultihashBytes :: ByteString -> Either String Cid
cidFromMultihashBytes bs
  | isV0 = newCidV0 <$> Multihash.decodeDigest bs
  | otherwise = bimap _3 _3 . Binary.runGetOrFail getCid $ LBS.fromStrict bs
  where
    isV0 = BS.length bs == 34 && magic == BS.take 2 bs
    magic = BS.pack [18, 32]

    _3 (_, _, x) = x

-- | Deserialise a 'Cid' in the 'Binary.Get' monad.
--
-- Note that this does __/not/__ support 'V0' 'Cid's.
getCid :: Binary.Get Cid
getCid = do
  cidVersion <- do
    v <- Binary.getWord8 >>= getVarInt
    if v < minVersion || v > maxVersion
      then fail $ "Cid: Invalid version: " <> show v
      else pure $ toEnum v

  case cidVersion of
    V1 -> do
      cidCodec <- do
        c <- Binary.getWord8 >>= getVarInt
        maybe (fail ("Cid: Unknown Codec: " <> show c)) pure $
          codecFromCode c
      cidHash <- Multihash.getMultihash
      pure Cid {..}
    v -> fail $ "Cid: Unsupported version: " <> show v
  where
    maxVersion = fromEnum (maxBound :: Version)
    minVersion = fromEnum (minBound :: Version)

cidFromMultibaseBytes :: ByteString -> Either String Cid
cidFromMultibaseBytes bs = do
  mb <- Multibase.decode bs
  cid <- cidFromMultihashBytes mb
  return cid

-- | Decode a 'Cid' from a textual representation.
--
-- The 'Text' value is expected to be base58 (bitcoin) encoded (for 'V0'
-- 'Cid's), or a valid 'Multibase'.
--
-- @
--    cidFromText . cidToText ≡ id
-- @
cidFromText :: Text -> Either String Cid
cidFromText t = decodeBase >=> cidFromMultihashBytes $ encodeUtf8 t
  where
    isV0 = Text.length t == 46 && "Qm" `Text.isPrefixOf` t

    decodeBase
      | isV0 = BaseN.decodeBase58btc
      | otherwise = Multibase.decode >=> guardReserved

    -- "If the first decoded byte is 0x12, return an error. Cidv0 Cids may not
    -- be multibase encoded and there will be no Cidv18 (0x12 = 18) to prevent
    -- ambiguity with decoded Cidv0s."
    guardReserved bs = case BS.uncons bs of
      Just (x, _) | x == 18 -> Left "Cid > V0 starts with reserved byte 0x12"
      _ -> Right bs

cidFromText' :: Text -> Cid
cidFromText' t = case cidFromText t of
  Left e -> error e
  Right x -> x

-- | Encode a 'Cid' to a textual representation.
--
-- The result is either a base58 (bitcoin) encoded string of just the 'cidHash'
-- value for 'V0' 'Cid's, or otherwise a 'Multibase' value at base 'Base32'
-- of the binary representation of the 'Cid' (as produced by 'buildCid').
cidToText :: Cid -> Text
cidToText cid =
  decodeUtf8 $
    case cidVersion cid of
      V0 ->
        BaseN.encodedBytes
          . BaseN.encodeBase58btc
          . Multihash.encodedBytes
          $ cidHash cid
      V1 ->
        Multibase.fromMultibase
          . Multibase.encode
          . BaseN.encodeAtBase BaseN.Base32
          . LBS.toStrict
          . Builder.toLazyByteString
          $ buildCid cid

-- | <https://github.com/multiformats/multicodec multicodec> numerical code of
-- the given 'Codec'.
codecToCode :: Codec -> Word8
codecToCode Raw = 0x55
codecToCode DagProtobuf = 0x70
codecToCode DagCbor = 0x71
codecToCode GitRaw = 0x78

-- | Attempt to convert from a <https://github.com/multiformats/multicodec multicodec>
-- numerical code to a 'Codec'.
codecFromCode :: Word8 -> Maybe Codec
codecFromCode 0x55 = pure Raw
codecFromCode 0x70 = pure DagProtobuf
codecFromCode 0x71 = pure DagCbor
codecFromCode 0x78 = pure GitRaw
codecFromCode _ = Nothing

encodeCid :: Cid -> Encoding
encodeCid cid =
  let cid_bytestring = LBS.toStrict $ Builder.toLazyByteString $ buildCid cid
      cid_atBaseID = BaseN.encodeAtBase BaseN.BaseIdentity cid_bytestring
      cid_multibase = Multibase.fromMultibase $ Multibase.encode cid_atBaseID
   in encodeTag 42 <> encodeBytes cid_multibase

cidToMultibaseBytes :: (Multibase.ToCode b) => BaseN.Base b -> Cid -> ByteString
cidToMultibaseBytes base cid =
  let cid_bytestring = LBS.toStrict $ Builder.toLazyByteString $ buildCid cid
      cid_atBase = BaseN.encodeAtBase base cid_bytestring
      cid_multibase = Multibase.fromMultibase $ Multibase.encode cid_atBase
   in cid_multibase

buildCidStrict :: Cid -> ByteString
buildCidStrict cid = LBS.toStrict $ Builder.toLazyByteString $ buildCid cid

decodeCid :: Decoder s Cid
decodeCid = do
  tag <- decodeTag
  if tag /= 42
    then fail "CBOR Link tag not equal to 42"
    else do
      base <- decodeBytes
      case cidFromMultibaseBytes base of
        Left str -> fail $ "Cid decoding error: " ++ str
        Right cid -> return cid

instance Serialise Cid where
  encode = encodeCid
  decode = decodeCid

hashLazyWith :: C.HashAlgorithm alg => alg -> LBS.ByteString -> C.Digest alg
hashLazyWith _ bs = C.hashlazy bs

mkCborCidV1 :: (Multihashable alg, Serialise a) => alg -> a -> Cid
mkCborCidV1 alg a = newCidV1 DagCbor (hashLazyWith alg (serialise a))

makeCid :: Serialise a => a -> Cid
makeCid a = mkCborCidV1 C.Blake2b_256 a

makeCidFromBytes :: LBS.ByteString -> Cid
makeCidFromBytes bs = newCidV1 DagCbor (hashLazyWith C.Blake2b_256 bs)
