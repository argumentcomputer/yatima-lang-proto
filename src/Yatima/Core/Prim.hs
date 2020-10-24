{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.Core.Prim where

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Word
import           Data.Int
import           Data.Bits
import           Data.Char
import           Data.FileEmbed

import           Numeric.IEEE

import           Yatima.Core.Hoas
import           Yatima.Core.Wasm
import           Yatima.Term
import           Yatima.QuasiQuoter

reduceOpr :: PrimOp -> Hoas -> Hoas
reduceOpr op arg = case (op,arg) of
  (I32_const,    LitH (VI32 a))    -> LamH "x" $ \x -> (LitH (VI32 a))
  (I64_const,    LitH (VI64 a))    -> LamH "x" $ \x -> (LitH (VI64 a))
  (F32_const,    LitH (VF32 a))    -> LamH "x" $ \x -> (LitH (VF32 a))
  (F64_const,    LitH (VF64 a))    -> LamH "x" $ \x -> (LitH (VF64 a))
  (I32_eqz,      LitH (VI32 a))    -> LitH (bool (a == 0))
  (I32_eq,       LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a == b))
    _                 -> noredex x
  (I32_ne,       LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a /= b))
    _                 -> noredex x
  (I32_lt_s,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (i32 a < i32 b))
    _                 -> noredex x
  (I32_lt_u,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a < b))
    _                 -> noredex x
  (I32_gt_s,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (i32 a > i32 b))
    _                 -> noredex x
  (I32_gt_u,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a > b))
    _                 -> noredex x
  (I32_le_s,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (i32 a <= i32 b))
    _                 -> noredex x
  (I32_le_u,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a <= b))
    _                 -> noredex x
  (I32_ge_s,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (i32 a >= i32 b))
    _                 -> noredex x
  (I32_ge_u,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (bool (a >= b))
    _                 -> noredex x
  (I64_eqz,      LitH (VI64 a))    -> LitH (bool (a == 0))
  (I64_eq,       LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a == b))
    _                 -> noredex x
  (I64_ne,       LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a /= b))
    _                 -> noredex x
  (I64_lt_s,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (i64 a < i64 b))
    _                 -> noredex x
  (I64_lt_u,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a < b))
    _                 -> noredex x
  (I64_gt_s,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (i64 a > i64 b))
    _                 -> noredex x
  (I64_gt_u,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a > b))
    _                 -> noredex x
  (I64_le_s,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (i64 a <= i64 b))
    _                 -> noredex x
  (I64_le_u,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a <= b))
    _                 -> noredex x
  (I64_ge_s,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (i64 a >= i64 b))
    _                 -> noredex x
  (I64_ge_u,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (bool (a >= b))
    _                 -> noredex x
  (F32_eq,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a == b))
    _                 -> noredex x
  (F32_ne,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a /= b))
    _                 -> noredex x
  (F32_lt,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a < b))
    _                 -> noredex x
  (F32_gt,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a > b))
    _                 -> noredex x
  (F32_le,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a <= b))
    _                 -> noredex x
  (F32_ge,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (bool (a >= b))
    _                 -> noredex x
  (F64_eq,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a == b))
    _                 -> noredex x
  (F64_ne,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a /= b))
    _                 -> noredex x
  (F64_lt,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a < b))
    _                 -> noredex x
  (F64_gt,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a > b))
    _                 -> noredex x
  (F64_le,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a <= b))
    _                 -> noredex x
  (F64_ge,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (bool (a >= b))
    _                 -> noredex x
  (I32_clz,     LitH (VI32 a))    -> LitH (VI32 $ cst32 $ countLeadingZeros a)
  (I32_ctz,     LitH (VI32 a))    -> LitH (VI32 $ cst32 $ countTrailingZeros a)
  (I32_popcnt,  LitH (VI32 a))    -> LitH (VI32 $ cst32 $ popCount a)
  (I32_add,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a + b))
    _                 -> noredex x
  (I32_sub,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a - b))
    _                 -> noredex x
  (I32_mul,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a * b))
    _                 -> noredex x
  (I32_div_s,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     ->
      if (b == 0 || (a == 0x80000000 && b == 0xFFFFFFFF))
      then noredex x
      else LitH (VI32 (u32 $ i32 a `quot` i32 b))
    _                 -> noredex x
  (I32_div_u,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> if b == 0 then noredex x else LitH (VI32 (a `quot` b))
    _                 -> noredex x
  (I32_rem_s,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> 
      if b == 0
      then noredex x 
      else LitH (VI32 (u32 $ i32 a `rem` i32 b))
    _                 -> noredex x
  (I32_rem_u,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> if b == 0 then noredex x else LitH (VI32 (a `rem` b))
    _                 -> noredex x
  (I32_and,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a .&. b))
    _                 -> noredex x
  (I32_or,      LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a .|. b))
    _                 -> noredex x
  (I32_xor,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a `xor` b))
    _                 -> noredex x
  (I32_shl,     LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a `shiftL` (fromIntegral b `rem` 32)))
    _                 -> noredex x
  (I32_shr_u,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a `shiftR` (fromIntegral b `rem` 32)))
    _                 -> noredex x
  (I32_shr_s,   LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     ->
      LitH (VI32 (u32 $ i32 a `shiftR` (fromIntegral b `rem` 32)))
    _                 -> noredex x
  (I32_rotl,    LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a `rotateL` fromIntegral b))
    _                 -> noredex x
  (I32_rotr,    LitH (VI32 a))    -> LamH "x" $ \x -> case x of
    LitH (VI32 b)     -> LitH (VI32 (a `rotateR` fromIntegral b))
    _                 -> noredex x
  (I64_clz,     LitH (VI64 a))    -> LitH (VI64 $ cst64 $ countLeadingZeros a)
  (I64_ctz,     LitH (VI64 a))    -> LitH (VI64 $ cst64 $ countTrailingZeros a)
  (I64_popcnt,  LitH (VI64 a))    -> LitH (VI64 $ cst64 $ popCount a)
  (I64_add,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a + b))
    _                 -> noredex x
  (I64_sub,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a - b))
    _                 -> noredex x
  (I64_mul,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a * b))
    _                 -> noredex x
  (I64_div_s,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     ->
      if (b == 0 || (a == 0x8000000000000000 && b == 0xFFFFFFFFFFFFFFFF))
      then noredex x
      else LitH (VI64 (u64 $ i64 a `quot` i64 b))
    _                 -> noredex x
  (I64_div_u,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> if b == 0 then noredex x else LitH (VI64 (a `quot` b))
    _                 -> noredex x
  (I64_rem_s,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> 
      if b == 0
      then noredex x 
      else LitH (VI64 (u64 $ i64 a `rem` i64 b))
    _                 -> noredex x
  (I64_rem_u,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> if b == 0 then noredex x else LitH (VI64 (a `rem` b))
    _                 -> noredex x
  (I64_and,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a .&. b))
    _                 -> noredex x
  (I64_or,      LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a .|. b))
    _                 -> noredex x
  (I64_xor,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a `xor` b))
    _                 -> noredex x
  (I64_shl,     LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a `shiftL` (fromIntegral b `rem` 64)))
    _                 -> noredex x
  (I64_shr_u,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a `shiftR` (fromIntegral b `rem` 64)))
    _                 -> noredex x
  (I64_shr_s,   LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     ->
      LitH (VI64 (u64 $ i64 a `shiftR` (fromIntegral b `rem` 64)))
    _                 -> noredex x
  (I64_rotl,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a `rotateL` fromIntegral b))
    _                 -> noredex x
  (I64_rotr,    LitH (VI64 a))    -> LamH "x" $ \x -> case x of
    LitH (VI64 b)     -> LitH (VI64 (a `rotateR` fromIntegral b))
    _                 -> noredex x
  (F32_abs,     LitH (VF32 a))    -> LitH (VF32 $ abs a)
  (F32_neg,     LitH (VF32 a))    -> LitH (VF32 $ negate a)
  (F32_ceil,    LitH (VF32 a))    -> LitH (VF32 $ floatCeil a)
  (F32_floor,   LitH (VF32 a))    -> LitH (VF32 $ floatFloor a)
  (F32_trunc,   LitH (VF32 a))    -> LitH (VF32 $ floatTrunc a)
  (F32_nearest, LitH (VF32 a))    -> LitH (VF32 $ nearest a)
  (F32_sqrt,    LitH (VF32 a))    -> LitH (VF32 $ sqrt a)
  (F32_add,     LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a + b))
    _                 -> noredex x
  (F32_sub,     LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a - b))
    _                 -> noredex x
  (F32_mul,     LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a * b))
    _                 -> noredex x
  (F32_div,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a / b))
    _                 -> noredex x
  (F32_min,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a `zeroAwareMin` b))
    _                 -> noredex x
  (F32_max,      LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a `zeroAwareMax` b))
    _                 -> noredex x
  (F32_copysign, LitH (VF32 a))    -> LamH "x" $ \x -> case x of
    LitH (VF32 b)     -> LitH (VF32 (a `copySign` b))
    _                 -> noredex x
  (F64_abs,      LitH (VF64 a))    -> LitH (VF64 $ abs a)
  (F64_neg,      LitH (VF64 a))    -> LitH (VF64 $ negate a)
  (F64_ceil,     LitH (VF64 a))    -> LitH (VF64 $ doubleCeil a)
  (F64_floor,    LitH (VF64 a))    -> LitH (VF64 $ doubleFloor a)
  (F64_trunc,    LitH (VF64 a))    -> LitH (VF64 $ doubleTrunc a)
  (F64_nearest,  LitH (VF64 a))    -> LitH (VF64 $ nearest a)
  (F64_sqrt,     LitH (VF64 a))    -> LitH (VF64 $ sqrt a)
  (F64_add,      LitH (VF64 a))    -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a + b))
    _                 -> noredex x
  (F64_sub,      LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a - b))
    _                 -> noredex x
  (F64_mul,      LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a * b))
    _                 -> noredex x
  (F64_div,      LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a / b))
    _                 -> noredex x
  (F64_min,      LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a `zeroAwareMin` b))
    _                 -> noredex x
  (F64_max,      LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a `zeroAwareMax` b))
    _                 -> noredex x
  (F64_copysign, LitH (VF64 a))     -> LamH "x" $ \x -> case x of
    LitH (VF64 b)     -> LitH (VF64 (a `copySign` b))
    _                 -> noredex x
  (I32_wrap_I64, LitH (VI64 a))      ->
    LitH (VI32 $ (fromIntegral $ a .&. 0xFFFFFFFF))
  (I32_trunc_F32_s, LitH (VF32 a))   ->
     if (isNaN a || isInfinite a || a >= 2^31 || a < -2^31) then noredex'
     else LitH (VI32 (u32 $ truncate a))
  (I32_trunc_F32_u, LitH (VF32 a))   ->
     if (isNaN a || isInfinite a || a >= 2^32 || a <= -1) then noredex'
     else LitH (VI32 (truncate a))
  (I32_trunc_F64_s, LitH (VF64 a))   ->
     if (isNaN a || isInfinite a || a >= 2^63 || a < -2^63) then noredex'
     else LitH (VI32 (u32 $ truncate a))
  (I32_trunc_F64_u, LitH (VF64 a))   ->
     if (isNaN a || isInfinite a || a >= 2^64 || a <= -1) then noredex'
     else LitH (VI32 (truncate a))
  (I64_extend_I32_s, LitH (VI32 a))  -> LitH (VI64 (u64 $ fromIntegral $ i32 a))
  (I64_extend_I32_u, LitH (VI32 a))  -> LitH (VI64 (fromIntegral a))
  (I64_trunc_F32_s, LitH (VF32 a))   ->
     if (isNaN a || isInfinite a || a >= 2^31 || a < -2^31) then noredex'
     else LitH (VI64 (u64 $ truncate a))
  (I64_trunc_F32_u, LitH (VF32 a))   ->
     if (isNaN a || isInfinite a || a >= 2^32 || a <= -1) then noredex'
     else LitH (VI64 (truncate a))
  (I64_trunc_F64_s, LitH (VF64 a))   ->
     if (isNaN a || isInfinite a || a >= 2^63 || a < -2^63) then noredex'
     else LitH (VI64 (u64 $ truncate a))
  (I64_trunc_F64_u, LitH (VF64 a))   ->
     if (isNaN a || isInfinite a || a >= 2^64 || a <= -1) then noredex'
     else LitH (VI64 (truncate a))
  (F32_convert_I32_s, LitH (VI32 a)) -> LitH (VF32 (realToFrac $ i32 a))
  (F32_convert_I32_u, LitH (VI32 a)) -> LitH (VF32 (realToFrac a))
  (F32_convert_I64_s, LitH (VI64 a)) -> LitH (VF32 (realToFrac $ i64 a))
  (F32_convert_I64_u, LitH (VI64 a)) -> LitH (VF32 (realToFrac a))
  (F32_demote_F64, LitH (VF64 a))    -> LitH (VF32 (realToFrac a))
  (F64_convert_I32_s, LitH (VI32 a)) -> LitH (VF64 (realToFrac $ i32 a))
  (F64_convert_I32_u, LitH (VI32 a)) -> LitH (VF64 (realToFrac a))
  (F64_convert_I64_s, LitH (VI64 a)) -> LitH (VF64 (realToFrac $ i64 a))
  (F64_convert_I64_u, LitH (VI64 a)) -> LitH (VF64 (realToFrac a))
  (F64_promote_F32, LitH (VF32 a))   -> LitH (VF64 (realToFrac a))
  (I32_reinterpret_F32, LitH (VF32 a)) -> LitH (VI32 (floatToWord a))
  (I64_reinterpret_F64, LitH (VF64 a)) -> LitH (VI64 (doubleToWord a))
  (F32_reinterpret_I32, LitH (VI32 a)) -> LitH (VF32 (wordToFloat a))
  (F64_reinterpret_I64, LitH (VI64 a)) -> LitH (VF64 (wordToDouble a))
  (Natural_succ, LitH (VNatural n)) -> LitH (VNatural $ n+1)
  (Natural_pred, LitH (VNatural 0)) -> LitH (VNatural 0)
  (Natural_pred, LitH (VNatural n)) -> LitH (VNatural $ n-1)
  (Natural_add,  LitH (VNatural a)) -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (VNatural $ a + b)
    _                 -> noredex x
  (Natural_sub,  LitH (VNatural a)) -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (VNatural $ ite (a < b) 0 (a - b))
    _                 -> noredex x
  (Natural_mul,  LitH (VNatural a)) -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (VNatural $ a * b)
    _                 -> noredex x
  (Natural_div, LitH (VNatural a))  -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (VNatural $ a `div` b)
    _                 -> noredex x
  (Natural_mod, LitH (VNatural a))  -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (VNatural $ a `mod` b)
    _                 -> noredex x
  (Natural_gt, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a > b))
    _                 -> noredex x
  (Natural_ge, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a >= b))
    _                 -> noredex x
  (Natural_eq, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a == b))
    _                 -> noredex  x
  (Natural_ne, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a /= b))
    _                 -> noredex  x
  (Natural_lt, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a < b))
    _                 -> noredex  x
  (Natural_le, LitH (VNatural a))   -> LamH "x" $ \x -> case x of
    LitH (VNatural b) -> LitH (bool (a <= b))
    _                 -> noredex  x
  (Natural_to_I64, LitH (VNatural a)) ->
    if a >= 2^64 then noredex' else LitH $ VI64 $ fromIntegral a
  (Natural_to_I32, LitH (VNatural a)) ->
    if a >= 2^32 then noredex' else LitH $ VI32 $ fromIntegral a
  (Natural_from_I64, LitH (VI64 a)) -> LitH $ VNatural $ fromIntegral a
  (Natural_from_I32, LitH (VI32 a)) -> LitH $ VNatural $ fromIntegral a
  (BitVector_cons, LitH (VI32 a))   -> LamH "x" $ \x -> case x of
    LitH (VBitVector n b) -> case a of
      0 -> LitH (VBitVector (n+1) (byteStringCons0 b))
      1 -> LitH (VBitVector (n+1) (byteStringCons1 b))
      _ -> noredex x
    _                 -> noredex x
  (BitVector_concat, LitH (VBitVector n a))   -> LamH "x" $ \x -> case x of
    LitH (VBitVector m b) -> LitH (VBitVector (n+m) $ a <> b)
    _                     -> noredex x
  (BitVector_length, LitH (VBitVector n a))   -> LitH (VNatural n)
  (String_cons, LitH (VChar c))    -> LamH "x" $ \x -> case x of
    LitH (VString txt) -> LitH (VString (T.cons c txt))
    _                 -> noredex x
  (String_concat, LitH (VString a))    -> LamH "x" $ \x -> case x of
    LitH (VString b) -> LitH (VString $ a <> b)
    _                 -> noredex x
  (Char_ord, LitH (VChar c))       -> LitH (VI64 $ fromIntegral $ ord c)
  (Char_chr, LitH (VI64 x))        -> LitH (VChar $ chr $ fromIntegral x)
  _                                 -> noredex'
  where
    noredex x = WhnH $ AppH (AppH (OprH op) arg) x
    noredex'  = WhnH $ AppH (OprH op) arg
    ite c t f = if c then t else f
    bool c = ite c (VI32 1) (VI32 0)
    cst32 :: Integral a => a -> Word32
    cst32 = fromIntegral
    cst64 :: Integral a => a -> Word64
    cst64 = fromIntegral
    i32 = asInt32
    i64 = asInt64
    u32 = asWord32
    u64 = asWord64

byteStringCons0 :: BS.ByteString -> BS.ByteString
byteStringCons0 bs = case BS.uncons bs of
  Nothing       -> BS.singleton 0
  Just (255,cs) -> BS.cons 0 $ BS.cons 255 $ cs
  Just (c,cs)   -> bs

byteStringCons1 :: BS.ByteString -> BS.ByteString
byteStringCons1 bs = case BS.uncons bs of
  Nothing       -> BS.singleton 1
  Just (255,cs) -> BS.cons 1 $ BS.cons 0 $ cs
  Just (c,cs)   -> BS.cons (c+1) bs

expandLit :: Literal -> Hoas
expandLit t = case t of
  VNatural nat ->
    if nat == 0
    then LamH "P" $ \p -> LamH "z" $ \z -> LamH "s" $ \s -> z
    else LamH "P" $ \p -> LamH "z" $ \z -> LamH "s" $ 
            \s -> AppH s (LitH $ VNatural (nat - 1))
  _        -> error "TODO"

litInduction :: LitType -> Hoas -> Hoas
litInduction t val = case t of
  TNatural ->
    AllH "P" None (AllH "" Many (LTyH TNatural) $ \_ -> TypH) $ \p ->
    AllH "" Affi (AppH p (LitH $ VNatural 0))$ \_ ->
    AllH "" Affi (AllH "pred" Many (LTyH TNatural) $
      \pred -> AppH p (AppH (OprH Natural_succ) pred)) $ \s ->
      AppH p val
  _    -> error "TODO"

foo :: Hoas
foo = termToHoas $ [yatima| λ x => x|]

typeOfLit :: Literal -> Hoas
typeOfLit t = case t of
  VWorld         -> LTyH TWorld
  VNatural _     -> LTyH TNatural
  VF64    _      -> LTyH TF64
  VF32    _      -> LTyH TF32
  VI64    _      -> LTyH TI64
  VI32    _      -> LTyH TI32
  VBitVector l _ -> LTyH (TBitVector l)
  VString _      -> LTyH TString
  VChar _        -> LTyH TChar

typeOfOpr :: PrimOp -> Hoas
typeOfOpr t = case t of
  Natural_succ -> AllH "" Many (LTyH TNatural) (\_ -> LTyH TNatural)
