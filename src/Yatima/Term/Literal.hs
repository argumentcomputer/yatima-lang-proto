{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Yatima.Term.Literal where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad
import Data.ByteString.Base64
import qualified Data.ByteString.Lazy as BSL
import Data.Data
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import Numeric.Natural

data Literal
  = VWorld
  | VNatural Natural
  | VF64 Double
  | VF32 Float
  | VI64 Word64
  | VI32 Word32
  | VBitVector Natural Natural
  | VString Text
  | VChar Char
  | VException
  deriving (Eq, Show, Data)

data LitType
  = TWorld
  | TNatural
  | TF64
  | TF32
  | TI64
  | TI32
  | TBitVector
  | TString
  | TChar
  | TException
  deriving (Eq, Show, Data)

encodeLiteral :: Literal -> Encoding
encodeLiteral t = case t of
  VWorld -> encodeListLen 2 <> ctor <> tag 0
  VNatural x -> encodeListLen 3 <> ctor <> tag 1 <> encode x
  VF64 x ->
    encodeListLen 3 <> ctor <> tag 2
      <> encodeString (serialiseBase64 x)
  VF32 x ->
    encodeListLen 3 <> ctor <> tag 3
      <> encodeString (serialiseBase64 x)
  VI64 x -> encodeListLen 3 <> ctor <> tag 4 <> encode x
  VI32 x -> encodeListLen 3 <> ctor <> tag 5 <> encode x
  VBitVector n x ->
    encodeListLen 4 <> ctor <> tag 6 <> encode n <> encode x
  VString x -> encodeListLen 3 <> ctor <> tag 7 <> encode x
  VChar x -> encodeListLen 3 <> ctor <> tag 8 <> encode x
  VException -> encodeListLen 2 <> ctor <> tag 9
  where
    serialiseBase64 :: forall a. Serialise a => a -> Text
    serialiseBase64 = encodeBase64 . BSL.toStrict . serialise @a
    ctor = encodeString "Lit"
    tag = encodeInt

decodeLiteral :: Decoder s Literal
decodeLiteral = do
  size <- decodeListLen
  ctor <- decodeString
  when (ctor /= "Lit") (fail $ "invalid Literal tag: " ++ show ctor)
  tag <- decodeInt
  case (size, tag) of
    (2, 0) -> pure VWorld
    (3, 1) -> VNatural <$> decode
    (3, 2) -> (VF64 . deserialiseBase64) <$> decodeString
    (3, 3) -> (VF32 . deserialiseBase64) <$> decodeString
    (3, 4) -> VI64 <$> decode
    (3, 5) -> VI32 <$> decode
    (4, 6) -> VBitVector <$> decode <*> decode
    (3, 7) -> VString <$> decode
    (3, 8) -> VChar <$> decode
    (2, 9) -> pure VException
    _ ->
      fail $
        concat
          ["invalid Literal with size: ", show size, " and tag: ", show tag]
  where
    deserialiseBase64 :: forall a. Serialise a => Text -> a
    deserialiseBase64 =
      deserialise @a . BSL.fromStrict . decodeBase64Lenient . T.encodeUtf8

instance Serialise Literal where
  encode = encodeLiteral
  decode = decodeLiteral

encodeLitType :: LitType -> Encoding
encodeLitType t = case t of
  TWorld -> encodeListLen 2 <> ctor <> tag 0
  TNatural -> encodeListLen 2 <> ctor <> tag 1
  TF64 -> encodeListLen 2 <> ctor <> tag 2
  TF32 -> encodeListLen 2 <> ctor <> tag 3
  TI64 -> encodeListLen 2 <> ctor <> tag 4
  TI32 -> encodeListLen 2 <> ctor <> tag 5
  TBitVector -> encodeListLen 2 <> ctor <> tag 6
  TString -> encodeListLen 2 <> ctor <> tag 7
  TChar -> encodeListLen 2 <> ctor <> tag 8
  TException -> encodeListLen 2 <> ctor <> tag 9
  where
    ctor = encodeString "LTy"
    tag = encodeInt

decodeLitType :: Decoder s LitType
decodeLitType = do
  size <- decodeListLen
  ctor <- decodeString
  when (ctor /= "LTy") (fail $ "invalid LitType tag: " ++ show ctor)
  tag <- decodeInt
  case (size, tag) of
    (2, 0) -> return TWorld
    (2, 1) -> return TNatural
    (2, 2) -> return TF64
    (2, 3) -> return TF32
    (2, 4) -> return TI64
    (2, 5) -> return TI32
    (2, 6) -> return TBitVector
    (2, 7) -> return TString
    (2, 8) -> return TChar
    (2, 9) -> return TException
    _ ->
      fail $
        concat
          ["invalid LitType with size: ", show size, " and tag: ", show tag]

instance Serialise LitType where
  encode = encodeLitType
  decode = decodeLitType
