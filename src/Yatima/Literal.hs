module Yatima.Literal where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word

import           Numeric.Natural

data Literal
  = VWorld
  | VNatural   Natural
  | VF64       Double
  | VF32       Float
  | VI64       Word64
  | VI32       Word32
  | VBitString ByteString
  | VBitVector Natural ByteString
  | VString    ByteString
  | VChar      Char
  deriving (Eq,Show)

data LiteralType
  = TWorld
  | TNatural
  | TF64
  | TF32
  | TI64
  | TI32
  | TBitString
  | TBitVector Natural
  | TString
  | TChar
  deriving (Eq,Show)

encodeLiteral :: Literal -> Encoding
encodeLiteral t = case t of
  VWorld         -> encodeListLen 1 <> ctor <> tag 0
  VNatural x     -> encodeListLen 2 <> ctor <> tag 1 <> encode x
  VF64 x         -> encodeListLen 2 <> ctor <> tag 2 <> encode x
  VF32 x         -> encodeListLen 2 <> ctor <> tag 3 <> encode x
  VI64 x         -> encodeListLen 2 <> ctor <> tag 4 <> encode x
  VI32 x         -> encodeListLen 2 <> ctor <> tag 5 <> encode x
  VBitString x   -> encodeListLen 2 <> ctor <> tag 6 <> encode x
  VBitVector n x -> encodeListLen 3 <> ctor <> tag 7 <> encode n <> encode x
  VString x      -> encodeListLen 2 <> ctor <> tag 8 <> encode x
  VChar x        -> encodeListLen 2 <> ctor <> tag 9 <> encode x
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
    (2,6) -> VBitString <$> decode
    (3,7) -> VBitVector <$> decode <*> decode
    (2,8) -> VString    <$> decode
    (2,9) -> VChar      <$> decode
    _     -> fail $ concat
       ["invalid Literal with size: ", show size, " and tag: ", show tag]

instance Serialise Literal where
  encode = encodeLiteral
  decode = decodeLiteral

encodeLiteralType :: LiteralType -> Encoding
encodeLiteralType t = case t of
  TWorld         -> encodeListLen 1 <> ctor <> tag 0
  TNatural       -> encodeListLen 1 <> ctor <> tag 1
  TF64           -> encodeListLen 1 <> ctor <> tag 2
  TF32           -> encodeListLen 1 <> ctor <> tag 3
  TI64           -> encodeListLen 1 <> ctor <> tag 4
  TI32           -> encodeListLen 1 <> ctor <> tag 5
  TBitString     -> encodeListLen 1 <> ctor <> tag 6
  TBitVector n   -> encodeListLen 2 <> ctor <> tag 7 <> encode n
  TString        -> encodeListLen 1 <> ctor <> tag 8
  TChar          -> encodeListLen 1 <> ctor <> tag 9
  where
    ctor = encodeString "LTy"
    tag  = encodeInt

decodeLiteralType :: Decoder s LiteralType
decodeLiteralType = do
  size <- decodeListLen
  ctor <- decodeString
  when (ctor /= "LTy") (fail $ "invalid LiteralType tag: " ++ show ctor)
  tag  <- decodeInt
  case (size,tag) of
    (1,0) -> return TWorld
    (1,1) -> return TNatural
    (1,2) -> return TF64
    (1,3) -> return TF32
    (1,4) -> return TI64
    (1,5) -> return TI32
    (1,6) -> return TBitString
    (2,7) -> TBitVector <$> decode
    (1,8) -> return TString
    (1,9) -> return TChar
    _     -> fail $ concat
       ["invalid Literal with size: ", show size, " and tag: ", show tag]

instance Serialise LiteralType where
  encode = encodeLiteralType
  decode = decodeLiteralType
