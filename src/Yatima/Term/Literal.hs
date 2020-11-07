{-# LANGUAGE DeriveDataTypeable #-}
module Yatima.Term.Literal where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Word
import           Data.Data

import           Numeric.Natural

data Literal
  = VWorld
  | VNatural   Natural
  | VF64       Double
  | VF32       Float
  | VI64       Word64
  | VI32       Word32
  | VBitVector Natural ByteString
  | VString    Text
  | VChar      Char
  | VException Text
  deriving (Eq,Show,Data)

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
  deriving (Eq,Show,Data)

encodeLiteral :: Literal -> Encoding
encodeLiteral t = case t of
  VWorld         -> encodeListLen 1 <> ctor <> tag 0
  VNatural x     -> encodeListLen 2 <> ctor <> tag 1 <> encode x
  VF64 x         -> encodeListLen 2 <> ctor <> tag 2 <> encode x
  VF32 x         -> encodeListLen 2 <> ctor <> tag 3 <> encode x
  VI64 x         -> encodeListLen 2 <> ctor <> tag 4 <> encode x
  VI32 x         -> encodeListLen 2 <> ctor <> tag 5 <> encode x
  VBitVector n x -> encodeListLen 3 <> ctor <> tag 6 <> encode n <> encode x
  VString x      -> encodeListLen 2 <> ctor <> tag 7 <> encode (T.encodeUtf8 x)
  VChar x        -> encodeListLen 2 <> ctor <> tag 8 <> encode x
  VException s   -> encodeListLen 2 <> ctor <> tag 9 <> encode s
  where
    ctor = encodeString "Lit"
    tag  = encodeInt

decodeLiteral :: Decoder s Literal
decodeLiteral = do
  size <- decodeListLen
  ctor <- decodeString
  when (ctor /= "Lit") (fail $ "invalid Literal tag: " ++ show ctor)
  tag  <- decodeInt
  case (size,tag) of
    (1,0) -> pure VWorld
    (2,1) -> VNatural   <$> decode
    (2,2) -> VF64       <$> decode
    (2,3) -> VF32       <$> decode
    (2,4) -> VI64       <$> decode
    (2,5) -> VI32       <$> decode
    (3,6) -> VBitVector <$> decode <*> decode
    (2,7) -> do
      bs <- decodeBytes
      case T.decodeUtf8' bs of
        Left  e -> fail $ "invalid Literal string with UTF-8 error: " ++ show e
        Right x -> return $ VString x
    (2,8) -> VChar      <$> decode
    (2,9) -> VException <$> decode
    _     -> fail $ concat
       ["invalid Literal with size: ", show size, " and tag: ", show tag]

instance Serialise Literal where
  encode = encodeLiteral
  decode = decodeLiteral

encodeLitType :: LitType -> Encoding
encodeLitType t = case t of
  TWorld     -> encodeListLen 1 <> ctor <> tag 0
  TNatural   -> encodeListLen 1 <> ctor <> tag 1
  TF64       -> encodeListLen 1 <> ctor <> tag 2
  TF32       -> encodeListLen 1 <> ctor <> tag 3
  TI64       -> encodeListLen 1 <> ctor <> tag 4
  TI32       -> encodeListLen 1 <> ctor <> tag 5
  TBitVector -> encodeListLen 1 <> ctor <> tag 6
  TString    -> encodeListLen 1 <> ctor <> tag 7
  TChar      -> encodeListLen 1 <> ctor <> tag 8
  TException -> encodeListLen 1 <> ctor <> tag 9
  where
    ctor = encodeString "LTy"
    tag  = encodeInt

decodeLitType :: Decoder s LitType
decodeLitType = do
  size <- decodeListLen
  ctor <- decodeString
  when (ctor /= "LTy") (fail $ "invalid LitType tag: " ++ show ctor)
  tag  <- decodeInt
  case (size,tag) of
    (1,0) -> return TWorld
    (1,1) -> return TNatural
    (1,2) -> return TF64
    (1,3) -> return TF32
    (1,4) -> return TI64
    (1,5) -> return TI32
    (1,6) -> return TBitVector
    (1,7) -> return TString
    (1,8) -> return TChar
    (1,9) -> return TException
    _     -> fail $ concat
       ["invalid LitType with size: ", show size, " and tag: ", show tag]

instance Serialise LitType where
  encode = encodeLitType
  decode = decodeLitType
