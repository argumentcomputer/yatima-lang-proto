module Yatima.Constant where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Data.Ratio
import qualified Data.ByteString            as BS
import           Data.ByteString            (ByteString)

import           Numeric.Natural

data Constant where
  CInt :: Integer    -> Constant
  CNat :: Natural    -> Constant
  CRat :: Rational   -> Constant
  CBit :: ByteString -> Constant
  CWrd :: Natural    -> Natural -> Constant
  CStr :: ByteString -> Constant
  CChr :: Char       -> Constant
  CUni :: Constant
  TInt :: Constant
  TNat :: Constant
  TRat :: Constant
  TBit :: Constant
  TWrd :: Integer    -> Constant
  TStr :: Constant
  TChr :: Constant
  TUni :: Constant

deriving instance Show Constant
deriving instance Eq Constant

encodeConstant :: Constant -> Encoding
encodeConstant t = case t of
  CInt x   -> encodeListLen 2 <> encodeInt 0 <> encodeInteger x
  CNat x   -> encodeListLen 2 <> encodeInt 1 <> encodeInteger (fromIntegral x)
  CRat x   -> encodeListLen 3 <> encodeInt 2
              <> encodeInteger (numerator x) <> encodeInteger (denominator x)
  CBit x   -> encodeListLen 2 <> encodeInt 3 <> encodeBytes x
  CWrd n x -> encodeListLen 3 <> encodeInt 4 <> encode n <> encode x
  CStr x   -> encodeListLen 2 <> encodeInt 5 <> encodeBytes x
  CChr x   -> encodeListLen 2 <> encodeInt 6 <> encode x
  CUni     -> encodeListLen 1 <> encodeInt 7
  TInt     -> encodeListLen 1 <> encodeInt 8
  TNat     -> encodeListLen 1 <> encodeInt 9
  TRat     -> encodeListLen 1 <> encodeInt 10
  TBit     -> encodeListLen 1 <> encodeInt 11
  TWrd x   -> encodeListLen 2 <> encodeInt 12 <> encodeInteger x
  TStr     -> encodeListLen 1 <> encodeInt 13
  TChr     -> encodeListLen 1 <> encodeInt 14
  TUni     -> encodeListLen 1 <> encodeInt 15

decodeConstant :: Decoder s Constant
decodeConstant = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (2, 0) -> CInt <$> decodeInteger
    (2, 1) -> CNat . fromIntegral  <$> decodeInteger
    (3, 2) -> CRat <$> ((%) <$> decodeInteger <*> decodeInteger)
    (2, 3) -> CBit <$> decodeBytes
    (3, 4) -> CWrd <$> decode <*> decode
    (2, 5) -> CStr <$> decodeBytes
    (2, 6) -> CChr <$> decode
    (1, 7) -> return CUni
    (1, 8) -> return TInt
    (1, 9) -> return TNat
    (1,10) -> return TRat
    (1,11) -> return TBit
    (2,12) -> TWrd <$> decodeInteger
    (1,13) -> return TStr
    (1,14) -> return TChr
    (1,15) -> return TUni
    _     -> fail $ concat
      ["invalid Constant with size: ", show size, " and tag: ", show tag]

instance Serialise Constant where
  encode = encodeConstant
  decode = decodeConstant
