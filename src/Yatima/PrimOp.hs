{-# LANGUAGE DeriveDataTypeable #-}
module Yatima.PrimOp where

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Data


-- WARNING: CHANGING PRIMITIVES BREAKS THE PACKAGE UNIVERSE
--
-- Changing PrimOp creates a new *incompatible* Yatima serialization
-- format version. Old serializations WILL NOT deserialize correctly.
-- Thus, changing this type requires a new *MAJOR* Yatima version number.
--
-- Don't touch PrimOp unless you're *sure* you know what you're doing.

data PrimOp
  -- WASMNumeric
  = I32_const
  | I64_const
  | F32_const
  | F64_const
  | I32_eqz
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
  -- Natural number
  | Natural_succ
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
  -- BitVector
  | BitVector_cons
  | BitVector_concat
  | BitVector_length
  -- String
  | String_cons
  | String_concat
  -- Char
  | Char_chr
  | Char_ord
  deriving (Eq,Ord,Show,Enum,Bounded,Data)

encodePrimOp :: PrimOp -> Encoding
encodePrimOp x = encodeListLen 2 <> encodeString "Opr" <> encodeInt (fromEnum x)

decodePrimOp :: Decoder s PrimOp
decodePrimOp = do
  size <- decodeListLen
  ctor <- decodeString
  when (size /=  2)    (fail $ "invalid PrimOp size: " ++ show size)
  when (ctor /= "Opr") (fail $ "invalid PrimOp tag: "  ++ show ctor)
  toEnum <$> decodeInt

instance Serialise PrimOp where
  encode = encodePrimOp
  decode = decodePrimOp

primOpName :: PrimOp -> Text
primOpName p = case p of
  I32_const          -> "I32_const"
  I64_const          -> "I64_const"
  F32_const          -> "F32_const"
  F64_const          -> "F64_const"
  I32_eqz            -> "I32_eqz"
  I32_eq             -> "I32_eq"
  I32_ne             -> "I32_ne"
  I32_lt_s           -> "I32_lt_s"
  I32_lt_u           -> "I32_lt_u"
  I32_gt_s           -> "I32_gt_s"
  I32_gt_u           -> "I32_gt_u"
  I32_le_s           -> "I32_le_s"
  I32_le_u           -> "I32_le_u"
  I32_ge_s           -> "I32_ge_s"
  I32_ge_u           -> "I32_ge_u"
  I64_eqz            -> "I64_eqz"
  I64_eq             -> "I64_eq"
  I64_ne             -> "I64_ne"
  I64_lt_s           -> "I64_lt_s"
  I64_lt_u           -> "I64_lt_u"
  I64_gt_s           -> "I64_gt_s"
  I64_gt_u           -> "I64_gt_u"
  I64_le_s           -> "I64_le_s"
  I64_le_u           -> "I64_le_u"
  I64_ge_s           -> "I64_ge_s"
  I64_ge_u           -> "I64_ge_u"
  F32_eq             -> "F32_eq"
  F32_ne             -> "F32_ne"
  F32_lt             -> "F32_lt"
  F32_gt             -> "F32_gt"
  F32_le             -> "F32_le"
  F32_ge             -> "F32_ge"
  F64_eq             -> "F64_eq"
  F64_ne             -> "F64_ne"
  F64_lt             -> "F64_lt"
  F64_gt             -> "F64_gt"
  F64_le             -> "F64_le"
  F64_ge             -> "F64_ge"
  I32_clz            -> "I32_clz"
  I32_ctz            -> "I32_ctz"
  I32_popcnt         -> "I32_popcnt"
  I32_add            -> "I32_add"
  I32_sub            -> "I32_sub"
  I32_mul            -> "I32_mul"
  I32_div_s          -> "I32_div_s"
  I32_div_u          -> "I32_div_u"
  I32_rem_s          -> "I32_rem_s"
  I32_rem_u          -> "I32_rem_u"
  I32_and            -> "I32_and"
  I32_or             -> "I32_or"
  I32_xor            -> "I32_xor"
  I32_shl            -> "I32_shl"
  I32_shr_s          -> "I32_shr_s"
  I32_shr_u          -> "I32_shr_u"
  I32_rotl           -> "I32_rotl"
  I32_rotr           -> "I32_rotr"
  I64_clz            -> "I64_clz"
  I64_ctz            -> "I64_ctz"
  I64_popcnt         -> "I64_popcnt"
  I64_add            -> "I64_add"
  I64_sub            -> "I64_sub"
  I64_mul            -> "I64_mul"
  I64_div_s          -> "I64_div_s"
  I64_div_u          -> "I64_div_u"
  I64_rem_s          -> "I64_rem_s"
  I64_rem_u          -> "I64_rem_u"
  I64_and            -> "I64_and"
  I64_or             -> "I64_or"
  I64_xor            -> "I64_xor"
  I64_shl            -> "I64_shl"
  I64_shr_s          -> "I64_shr_s"
  I64_shr_u          -> "I64_shr_u"
  I64_rotl           -> "I64_rotl"
  I64_rotr           -> "I64_rotr"
  F32_abs            -> "F32_abs"
  F32_neg            -> "F32_neg"
  F32_ceil           -> "F32_ceil"
  F32_floor          -> "F32_floor"
  F32_trunc          -> "F32_trunc"
  F32_nearest        -> "F32_nearest"
  F32_sqrt           -> "F32_sqrt"
  F32_add            -> "F32_add"
  F32_sub            -> "F32_sub"
  F32_mul            -> "F32_mul"
  F32_div            -> "F32_div"
  F32_min            -> "F32_min"
  F32_max            -> "F32_max"
  F32_copysign       -> "F32_copysign"
  F64_abs            -> "F64_abs"
  F64_neg            -> "F64_neg"
  F64_ceil           -> "F64_ceil"
  F64_floor          -> "F64_floor"
  F64_trunc          -> "F64_trunc"
  F64_nearest        -> "F64_nearest"
  F64_sqrt           -> "F64_sqrt"
  F64_add            -> "F64_add"
  F64_sub            -> "F64_sub"
  F64_mul            -> "F64_mul"
  F64_div            -> "F64_div"
  F64_min            -> "F64_min"
  F64_max            -> "F64_max"
  F64_copysign       -> "F64_copysign"
  I32_wrap_I64       -> "I32_wrap_I64"
  I32_trunc_F32_s    -> "I32_trunc_F32_s"
  I32_trunc_F32_u    -> "I32_trunc_F32_u"
  I32_trunc_F64_s    -> "I32_trunc_F64_s"
  I32_trunc_F64_u    -> "I32_trunc_F64_u"
  I64_extend_I32_s   -> "I64_extend_I32_s"
  I64_extend_I32_u   -> "I64_extend_I32_u"
  I64_trunc_F32_s    -> "I64_trunc_F32_s"
  I64_trunc_F32_u    -> "I64_trunc_F32_u"
  I64_trunc_F64_s    -> "I64_trunc_F64_s"
  I64_trunc_F64_u    -> "I64_trunc_F64_u"
  F32_convert_I32_s  -> "F32_convert_I32_s"
  F32_convert_I32_u  -> "F32_convert_I32_u"
  F32_convert_I64_s  -> "F32_convert_I64_s"
  F32_convert_I64_u  -> "F32_convert_I64_u"
  F32_demote_F64     -> "F32_demote_F64"
  F64_convert_I32_s  -> "F64_convert_I32_s"
  F64_convert_I32_u  -> "F64_convert_I32_u"
  F64_convert_I64_s  -> "F64_convert_I64_s"
  F64_convert_I64_u  -> "F64_convert_I64_u"
  F64_promote_F32    -> "F64_promote_F32"
  I32_reinterpret_F32-> "I32_reinterpret_F32"
  I64_reinterpret_F64-> "I64_reinterpret_F64"
  F32_reinterpret_I32-> "F32_reinterpret_I32"
  F64_reinterpret_I64-> "F64_reinterpret_I64"
  Natural_succ       -> "Natural_succ"
  Natural_pred       -> "Natural_pred"
  Natural_add        -> "Natural_add"
  Natural_mul        -> "Natural_mul"
  Natural_sub        -> "Natural_sub"
  Natural_div        -> "Natural_div"
  Natural_mod        -> "Natural_mod"
  Natural_gt         -> "Natural_gt"
  Natural_ge         -> "Natural_ge"
  Natural_eq         -> "Natural_eq"
  Natural_ne         -> "Natural_ne"
  Natural_lt         -> "Natural_lt"
  Natural_le         -> "Natural_le"
  Natural_to_I64     -> "Natural_to_I64"
  Natural_to_I32     -> "Natural_to_I32"
  Natural_from_I64   -> "Natural_from_I64"
  Natural_from_I32   -> "Natural_from_I32"
  BitVector_cons     -> "BitVector_cons"
  BitVector_concat   -> "BitVector_concat"
  BitVector_length   -> "BitVector_length"
  String_cons        -> "String_cons"
  String_concat      -> "String_concat"
  Char_chr           -> "Char_chr"
  Char_ord           -> "Char_ord"
