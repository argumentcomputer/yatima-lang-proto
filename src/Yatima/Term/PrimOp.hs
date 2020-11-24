{-# LANGUAGE DeriveDataTypeable #-}

module Yatima.Term.PrimOp where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad
import Data.Data
import Data.Text (Text)

-- WARNING: CHANGING PRIMITIVES BREAKS THE PACKAGE UNIVERSE
--
-- Changing PrimOp creates a new *incompatible* Yatima serialization
-- format version. Old serializations WILL NOT deserialize correctly.
-- Thus, changing this type requires a new *MAJOR* Yatima version number.
--
-- Don't touch PrimOp unless you're *sure* you know what you're doing.

data PrimOp
  = -- WASMNumeric
    I32_eqz
  | I32_eq
  | I32_ne
  | I32_lt_s
  | I32_lt_u
  | I32_gt_s
  | I32_gt_u
  | I32_le_s
  | I32_le_u
  | I32_ge_s
  | I32_ge_u
  | I64_eqz
  | I64_eq
  | I64_ne
  | I64_lt_s
  | I64_lt_u
  | I64_gt_s
  | I64_gt_u
  | I64_le_s
  | I64_le_u
  | I64_ge_s
  | I64_ge_u
  | F32_eq
  | F32_ne
  | F32_lt
  | F32_gt
  | F32_le
  | F32_ge
  | F64_eq
  | F64_ne
  | F64_lt
  | F64_gt
  | F64_le
  | F64_ge
  | I32_clz
  | I32_ctz
  | I32_popcnt
  | I32_add
  | I32_sub
  | I32_mul
  | I32_div_s
  | I32_div_u
  | I32_rem_s
  | I32_rem_u
  | I32_and
  | I32_or
  | I32_xor
  | I32_shl
  | I32_shr_s
  | I32_shr_u
  | I32_rotl
  | I32_rotr
  | I64_clz
  | I64_ctz
  | I64_popcnt
  | I64_add
  | I64_sub
  | I64_mul
  | I64_div_s
  | I64_div_u
  | I64_rem_s
  | I64_rem_u
  | I64_and
  | I64_or
  | I64_xor
  | I64_shl
  | I64_shr_s
  | I64_shr_u
  | I64_rotl
  | I64_rotr
  | F32_abs
  | F32_neg
  | F32_ceil
  | F32_floor
  | F32_trunc
  | F32_nearest
  | F32_sqrt
  | F32_add
  | F32_sub
  | F32_mul
  | F32_div
  | F32_min
  | F32_max
  | F32_copysign
  | F64_abs
  | F64_neg
  | F64_ceil
  | F64_floor
  | F64_trunc
  | F64_nearest
  | F64_sqrt
  | F64_add
  | F64_sub
  | F64_mul
  | F64_div
  | F64_min
  | F64_max
  | F64_copysign
  | I32_wrap_I64
  | I32_trunc_F32_s
  | I32_trunc_F32_u
  | I32_trunc_F64_s
  | I32_trunc_F64_u
  | I64_extend_I32_s
  | I64_extend_I32_u
  | I64_trunc_F32_s
  | I64_trunc_F32_u
  | I64_trunc_F64_s
  | I64_trunc_F64_u
  | F32_convert_I32_s
  | F32_convert_I32_u
  | F32_convert_I64_s
  | F32_convert_I64_u
  | F32_demote_F64
  | F64_convert_I32_s
  | F64_convert_I32_u
  | F64_convert_I64_s
  | F64_convert_I64_u
  | F64_promote_F32
  | I32_reinterpret_F32
  | I64_reinterpret_F64
  | F32_reinterpret_I32
  | F64_reinterpret_I64
  | -- Natural number
    Natural_succ
  | Natural_pred
  | Natural_add
  | Natural_mul
  | Natural_sub
  | Natural_div
  | Natural_mod
  | Natural_gt
  | Natural_ge
  | Natural_eq
  | Natural_ne
  | Natural_lt
  | Natural_le
  | Natural_to_I64
  | Natural_to_I32
  | Natural_from_I64
  | Natural_from_I32
  | -- BitVector
    BitVector_b0
  | BitVector_b1
  | BitVector_concat
  | BitVector_length
  | -- String
    String_cons
  | String_concat
  | -- Char
    Char_chr
  | Char_ord
  | -- Conversion primitives
    Char_to_U8
  | Char_from_U8
  | I32_to_U32
  | I32_from_U32
  | F32_to_U32
  | F32_from_U32
  | I64_to_U64
  | I64_from_U64
  | F64_to_U64
  | F64_from_U64
  deriving (Eq, Ord, Show, Enum, Bounded, Data)

encodePrimOp :: PrimOp -> Encoding
encodePrimOp x = encodeListLen 2 <> encodeString "Opr" <> encodeInt (fromEnum x)

decodePrimOp :: Decoder s PrimOp
decodePrimOp = do
  size <- decodeListLen
  ctor <- decodeString
  when (size /= 2) (fail $ "invalid PrimOp size: " ++ show size)
  when (ctor /= "Opr") (fail $ "invalid PrimOp tag: " ++ show ctor)
  toEnum <$> decodeInt

instance Serialise PrimOp where
  encode = encodePrimOp
  decode = decodePrimOp

primOpName :: PrimOp -> Text
primOpName p = case p of
  I32_eqz -> "I32.eqz"
  I32_eq -> "I32.eq"
  I32_ne -> "I32.ne"
  I32_lt_s -> "I32.lt_s"
  I32_lt_u -> "I32.lt_u"
  I32_gt_s -> "I32.gt_s"
  I32_gt_u -> "I32.gt_u"
  I32_le_s -> "I32.le_s"
  I32_le_u -> "I32.le_u"
  I32_ge_s -> "I32.ge_s"
  I32_ge_u -> "I32.ge_u"
  I64_eqz -> "I64.eqz"
  I64_eq -> "I64.eq"
  I64_ne -> "I64.ne"
  I64_lt_s -> "I64.lt_s"
  I64_lt_u -> "I64.lt_u"
  I64_gt_s -> "I64.gt_s"
  I64_gt_u -> "I64.gt_u"
  I64_le_s -> "I64.le_s"
  I64_le_u -> "I64.le_u"
  I64_ge_s -> "I64.ge_s"
  I64_ge_u -> "I64.ge_u"
  F32_eq -> "F32.eq"
  F32_ne -> "F32.ne"
  F32_lt -> "F32.lt"
  F32_gt -> "F32.gt"
  F32_le -> "F32.le"
  F32_ge -> "F32.ge"
  F64_eq -> "F64.eq"
  F64_ne -> "F64.ne"
  F64_lt -> "F64.lt"
  F64_gt -> "F64.gt"
  F64_le -> "F64.le"
  F64_ge -> "F64.ge"
  I32_clz -> "I32.clz"
  I32_ctz -> "I32.ctz"
  I32_popcnt -> "I32.popcnt"
  I32_add -> "I32.add"
  I32_sub -> "I32.sub"
  I32_mul -> "I32.mul"
  I32_div_s -> "I32.div_s"
  I32_div_u -> "I32.div_u"
  I32_rem_s -> "I32.rem_s"
  I32_rem_u -> "I32.rem_u"
  I32_and -> "I32.and"
  I32_or -> "I32.or"
  I32_xor -> "I32.xor"
  I32_shl -> "I32.shl"
  I32_shr_s -> "I32.shr_s"
  I32_shr_u -> "I32.shr_u"
  I32_rotl -> "I32.rotl"
  I32_rotr -> "I32.rotr"
  I64_clz -> "I64.clz"
  I64_ctz -> "I64.ctz"
  I64_popcnt -> "I64.popcnt"
  I64_add -> "I64.add"
  I64_sub -> "I64.sub"
  I64_mul -> "I64.mul"
  I64_div_s -> "I64.div_s"
  I64_div_u -> "I64.div_u"
  I64_rem_s -> "I64.rem_s"
  I64_rem_u -> "I64.rem_u"
  I64_and -> "I64.and"
  I64_or -> "I64.or"
  I64_xor -> "I64.xor"
  I64_shl -> "I64.shl"
  I64_shr_s -> "I64.shr_s"
  I64_shr_u -> "I64.shr_u"
  I64_rotl -> "I64.rotl"
  I64_rotr -> "I64.rotr"
  F32_abs -> "F32.abs"
  F32_neg -> "F32.neg"
  F32_ceil -> "F32.ceil"
  F32_floor -> "F32.floor"
  F32_trunc -> "F32.trunc"
  F32_nearest -> "F32.nearest"
  F32_sqrt -> "F32.sqrt"
  F32_add -> "F32.add"
  F32_sub -> "F32.sub"
  F32_mul -> "F32.mul"
  F32_div -> "F32.div"
  F32_min -> "F32.min"
  F32_max -> "F32.max"
  F32_copysign -> "F32.copysign"
  F64_abs -> "F64.abs"
  F64_neg -> "F64.neg"
  F64_ceil -> "F64.ceil"
  F64_floor -> "F64.floor"
  F64_trunc -> "F64.trunc"
  F64_nearest -> "F64.nearest"
  F64_sqrt -> "F64.sqrt"
  F64_add -> "F64.add"
  F64_sub -> "F64.sub"
  F64_mul -> "F64.mul"
  F64_div -> "F64.div"
  F64_min -> "F64.min"
  F64_max -> "F64.max"
  F64_copysign -> "F64.copysign"
  I32_wrap_I64 -> "I32.wrap_I64"
  I32_trunc_F32_s -> "I32.trunc_F32_s"
  I32_trunc_F32_u -> "I32.trunc_F32_u"
  I32_trunc_F64_s -> "I32.trunc_F64_s"
  I32_trunc_F64_u -> "I32.trunc_F64_u"
  I64_extend_I32_s -> "I64.extend_I32_s"
  I64_extend_I32_u -> "I64.extend_I32_u"
  I64_trunc_F32_s -> "I64.trunc_F32_s"
  I64_trunc_F32_u -> "I64.trunc_F32_u"
  I64_trunc_F64_s -> "I64.trunc_F64_s"
  I64_trunc_F64_u -> "I64.trunc_F64_u"
  F32_convert_I32_s -> "F32.convert_I32_s"
  F32_convert_I32_u -> "F32.convert_I32_u"
  F32_convert_I64_s -> "F32.convert_I64_s"
  F32_convert_I64_u -> "F32.convert_I64_u"
  F32_demote_F64 -> "F32.demote_F64"
  F64_convert_I32_s -> "F64.convert_I32_s"
  F64_convert_I32_u -> "F64.convert_I32_u"
  F64_convert_I64_s -> "F64.convert_I64_s"
  F64_convert_I64_u -> "F64.convert_I64_u"
  F64_promote_F32 -> "F64.promote_F32"
  I32_reinterpret_F32 -> "I32.reinterpret_F32"
  I64_reinterpret_F64 -> "I64.reinterpret_F64"
  F32_reinterpret_I32 -> "F32.reinterpret_I32"
  F64_reinterpret_I64 -> "F64.reinterpret_I64"
  Natural_succ -> "Natural.succ"
  Natural_pred -> "Natural.pred"
  Natural_add -> "Natural.add"
  Natural_mul -> "Natural.mul"
  Natural_sub -> "Natural.sub"
  Natural_div -> "Natural.div"
  Natural_mod -> "Natural.mod"
  Natural_gt -> "Natural.gt"
  Natural_ge -> "Natural.ge"
  Natural_eq -> "Natural.eq"
  Natural_ne -> "Natural.ne"
  Natural_lt -> "Natural.lt"
  Natural_le -> "Natural.le"
  Natural_to_I64 -> "Natural.to_I64"
  Natural_to_I32 -> "Natural.to_I32"
  Natural_from_I64 -> "Natural.from_I64"
  Natural_from_I32 -> "Natural.from_I32"
  BitVector_b0 -> "BitVector.b0"
  BitVector_b1 -> "BitVector.b1"
  BitVector_concat -> "BitVector.concat"
  BitVector_length -> "BitVector.length"
  String_cons -> "String.cons"
  String_concat -> "String.concat"
  Char_chr -> "Char.chr"
  Char_ord -> "Char.ord"
  Char_to_U8 -> "Char.to_U8"
  Char_from_U8 -> "Char.from_U8"
  I32_to_U32 -> "I32.to_U32"
  I32_from_U32 -> "I32.from_U32"
  F32_to_U32 -> "F32.to_U32"
  F32_from_U32 -> "F32.from_U32"
  I64_to_U64 -> "I64.to_U64"
  I64_from_U64 -> "I64.from_U64"
  F64_to_U64 -> "F64.to_U64"
  F64_from_U64 -> "F64.from_U64"
