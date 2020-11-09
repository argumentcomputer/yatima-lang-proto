{-
Module      : Data.IPLD.DagJSON
Description : This module implements DagJSON embedding of JSON structures into the InterPlanetary Linked Data (IPLD) directed acyclic graph (DAG)
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental


This module modifies work by [Joel
Burget](https://github.com/joelburget/haskell-ipld/blob/master/src/Network/IPLD/Internal.hs)
which is licensed under BSD3 terms included with this package in the
@licenses/2017_Joel_Burget@ file.

This module modifies work by [Duncan Coutts and Well-Typed
LLP](https://github.com/well-typed/cborg/blob/master/cborg-json/src/Codec/CBOR/JSON.hs))
which is licensed under BSD3 terms included with this package in the
@licenses/2017_Duncan_Coutts@ file.

-}

module Data.IPLD.DagJSON where

import           Data.Monoid
import           Control.Applicative
import           Prelude hiding (decodeFloat)

import           Codec.Serialise
import           Codec.CBOR.Encoding
import           Codec.CBOR.Decoding
import           Data.Aeson                          ((.=) )
import qualified Data.Aeson                          as Aeson
import qualified Data.HashMap.Lazy                   as HM
import           Data.Scientific                     as Scientific
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Text                           (Text)
import qualified Data.Vector                         as V
import           Data.IPLD.CID

data DagJSON
  = DagLink CID
  | DagObject (HM.HashMap Text DagJSON)
  | DagArray  (V.Vector DagJSON)
  | DagText   Text
  | DagNumber Scientific
  | DagBool   Bool
  | DagNull
  deriving (Eq, Show)

toAeson :: DagJSON -> Aeson.Value
toAeson val = case val of
  DagLink   cid  -> Aeson.object [ "/" .= cidToText cid]
  DagObject hmap -> Aeson.Object (toAeson <$> hmap)
  DagArray  arr  -> Aeson.Array  (toAeson <$> arr)
  DagText   text -> Aeson.String text
  DagNumber num  -> Aeson.Number num
  DagBool   bool -> Aeson.Bool   bool
  DagNull        -> Aeson.Null

fromAeson :: Aeson.Value -> DagJSON
fromAeson val = case val of
  Aeson.Object hmap -> 
    case (HM.size hmap == 1, HM.lookup "/" hmap) of
      (True, Just (Aeson.String text)) -> case cidFromText text of
        Right c -> DagLink c
        _       -> DagObject (fromAeson <$> hmap)
      _                                -> DagObject (fromAeson <$> hmap)
  Aeson.Array  arr                -> DagArray  (fromAeson <$> arr)
  Aeson.String text               -> DagText   text
  Aeson.Number num                -> DagNumber num
  Aeson.Bool   bool               -> DagBool   bool
  Aeson.Null                      -> DagNull

instance Aeson.FromJSON DagJSON where
  parseJSON a = return $ fromAeson a

instance Aeson.ToJSON DagJSON where
  toJSON = toAeson

-- | Encode a DagJSON value into CBOR.
encodeDagJSON :: DagJSON -> Encoding
encodeDagJSON x = case x of
  DagLink c    -> encodeCid c
  DagObject vs -> encodeObject vs
  DagArray  vs -> encodeArray  vs
  DagText s    -> encodeString s
  DagNumber n  -> case Scientific.floatingOrInteger n of
                    Left  d -> encodeDouble  d
                    Right i -> encodeInteger i
  DagBool   b  -> encodeBool b
  DagNull      -> encodeNull

encodeObject :: HM.HashMap Text DagJSON -> Encoding
encodeObject vs =
    encodeMapLen (fromIntegral (HM.size vs))
 <> HM.foldrWithKey (\k v r -> encodeString k <> encodeDagJSON v <> r) mempty vs

encodeArray :: V.Vector DagJSON -> Encoding
encodeArray vs =
    encodeListLen (fromIntegral (V.length vs))
 <> V.foldr (\v r -> encodeDagJSON v <> r) mempty vs

-- | Decode an arbitrary CBOR value into DagJSON.
decodeDagJSON :: Decoder s DagJSON
decodeDagJSON = do
    tkty <- peekTokenType
    case tkty of
      TypeUInt    -> decodeNumberIntegral
      TypeUInt64  -> decodeNumberIntegral
      TypeNInt    -> decodeNumberIntegral
      TypeNInt64  -> decodeNumberIntegral
      TypeInteger -> decodeNumberIntegral
      TypeFloat16 -> decodeNumberFloat16
      TypeFloat32 -> decodeNumberFloating
      TypeFloat64 -> decodeNumberFloating
      TypeBool    -> DagBool <$> decodeBool
      TypeNull    -> DagNull <$  decodeNull
      TypeString  -> DagText <$> decodeString
      TypeBytes   -> DagText . T.decodeUtf8 <$> decodeBytes
      TypeTag     -> DagLink <$> decodeCid
      TypeListLen      -> decodeListLen >>= decodeListN
      TypeListLenIndef -> decodeListLenIndef >> decodeListIndef []
      TypeMapLen       -> decodeMapLen >>= flip decodeMapN HM.empty
      _           -> fail $ "unexpected CBOR token type for a JSON value: "
                         ++ show tkty

decodeNumberIntegral :: Decoder s DagJSON
decodeNumberIntegral = DagNumber . fromInteger <$> decodeInteger

decodeNumberFloating :: Decoder s DagJSON
decodeNumberFloating = DagNumber . Scientific.fromFloatDigits <$> decodeDouble

decodeNumberFloat16 :: Decoder s DagJSON
decodeNumberFloat16 = do
    f <- decodeFloat
    if isNaN f || isInfinite f
        then return DagNull
        else return $ DagNumber (Scientific.fromFloatDigits f)

decodeListN :: Int -> Decoder s DagJSON
decodeListN n = do
  vec <- V.replicateM n decodeDagJSON
  return $ DagArray vec

decodeListIndef :: [DagJSON] -> Decoder s DagJSON
decodeListIndef acc = do
    stop <- decodeBreakOr
    if stop then return $ DagArray (V.fromList (reverse acc))
            else do tm <- decodeDagJSON
                    decodeListIndef (tm : acc)

decodeMapN :: Int -> HM.HashMap Text DagJSON -> Decoder s DagJSON
decodeMapN n acc =
    case n of
      0 -> return $ DagObject acc
      _ -> do
        tk <- decodeString
        tv <- decodeDagJSON
        decodeMapN (n-1) (HM.insert tk tv acc)

instance Serialise DagJSON where
  encode = encodeDagJSON
  decode = decodeDagJSON
