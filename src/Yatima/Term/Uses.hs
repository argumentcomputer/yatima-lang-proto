{-# LANGUAGE DeriveDataTypeable #-}
module Yatima.Term.Uses where

import           Data.Data
import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

data Uses = None | Affi | Once | Many deriving (Eq, Show, Enum, Data)

(+#) :: Uses -> Uses -> Uses
None +# x    = x
x    +# None = x
x    +# y    = Many

(*#) :: Uses -> Uses -> Uses
None *# x    = None
Affi *# None = None
Affi *# Affi = Affi
Affi *# Once = Affi
Affi *# Many = Many
Once *# x    = x
Many *# None = None
Many *# x    = Many

(≤#) :: Uses -> Uses -> Bool
None ≤# Once = False
None ≤# x    = True
Affi ≤# None = False
Affi ≤# Once = False
Affi ≤# x    = True
Once ≤# None = False
Once ≤# x    = True
Many ≤# Many = True
Many ≤# x    = False


(>#) :: Uses -> Uses -> Bool
(>#) x y = not (x ≤# y)


encodeUses :: Uses -> Encoding
encodeUses u = case u of
  None -> encodeInt 0
  Affi -> encodeInt 1
  Once -> encodeInt 2
  Many -> encodeInt 3

decodeUses :: Decoder s Uses
decodeUses = do
  tag  <- decodeInt
  case tag of
    0 -> return None
    1 -> return Affi
    2 -> return Once
    3 -> return Many
    _ -> fail $ "invalid Uses tag: " ++ show tag

instance Serialise Uses where
  encode = encodeUses
  decode = decodeUses
