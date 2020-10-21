module Yatima.Literal where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word

import           Numeric.Natural

data Literal
  = VWorld
  | TWorld
  | VNatural   Natural
  | TNatural
  | VF64       Double
  | TF64
  | VF32       Float
  | TF32
  | VI64       Word64
  | TI64
  | VI32       Word32
  | TI32
  | VBitString ByteString
  | TBitString
  | VBitVector Natural ByteString
  | TBitVector Natural
  | VString    ByteString
  | TString
  | VChar      Char
  | TChar

deriving instance Show Literal
deriving instance Eq Literal

encodeLiteral :: Literal -> Encoding
encodeLiteral t = case t of
  VWorld         -> encodeListLen 1 <> encodeInt  0
  TWorld         -> encodeListLen 1 <> encodeInt  1
  VNatural x     -> encodeListLen 2 <> encodeInt  2 <> encode x
  TNatural       -> encodeListLen 1 <> encodeInt  3
  VF64 x         -> encodeListLen 2 <> encodeInt  4 <> encode x
  TF64           -> encodeListLen 1 <> encodeInt  5
  VF32 x         -> encodeListLen 2 <> encodeInt  6 <> encode x
  TF32           -> encodeListLen 1 <> encodeInt  7
  VI64 x         -> encodeListLen 2 <> encodeInt  8 <> encode x
  TI64           -> encodeListLen 1 <> encodeInt  9
  VI32 x         -> encodeListLen 2 <> encodeInt 10 <> encode x
  TI32           -> encodeListLen 1 <> encodeInt 11
  VBitString x   -> encodeListLen 2 <> encodeInt 12 <> encode x
  TBitString     -> encodeListLen 1 <> encodeInt 13
  VBitVector n x -> encodeListLen 3 <> encodeInt 14 <> encode n <> encode x
  TBitVector n   -> encodeListLen 2 <> encodeInt 15 <> encode n
  VString x      -> encodeListLen 2 <> encodeInt 16 <> encode x
  TString        -> encodeListLen 1 <> encodeInt 17
  VChar x        -> encodeListLen 2 <> encodeInt 18 <> encode x
  TChar          -> encodeListLen 1 <> encodeInt 19

decodeLiteral :: Decoder s Literal
decodeLiteral = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (1, 0) -> return VWorld
    (1, 1) -> return TWorld
    (2, 2) -> VNatural <$> decode
    (1, 3) -> return TNatural
    (2, 4) -> VF64 <$> decode
    (1, 5) -> return TF64
    (2, 6) -> VF32 <$> decode
    (1, 7) -> return TF32
    (2, 8) -> VI64 <$> decode
    (1, 9) -> return TI64
    (2,10) -> VI32 <$> decode
    (1,11) -> return TI32
    (2,12) -> VBitString <$> decode
    (1,13) -> return TBitString
    (3,14) -> VBitVector <$> decode <*> decode
    (2,15) -> TBitVector <$> decode
    (2,16) -> VString <$> decode
    (1,17) -> return TString
    (2,18) -> VChar <$> decode
    (1,19) -> return TChar
    _     -> fail $ concat
       ["invalid Literal with size: ", show size, " and tag: ", show tag]

instance Serialise Literal where
  encode = encodeLiteral
  decode = decodeLiteral
