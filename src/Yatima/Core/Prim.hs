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
import qualified Yatima.Core.Ctx as Ctx
import           Yatima.Term
import           Yatima.QuasiQuoter

reduceOpr :: PrimOp -> [Hoas] -> Hoas
reduceOpr op args = case (op,args) of
  -- (I32_const,   LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 a)
  -- (I64_const,   LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 a)
  -- (F32_const,   LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (VF32 a)
  -- (F64_const,   LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (VF64 a)
  -- (I32_eqz,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a == 0))
  -- (I32_eq,      LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a == b))
  -- (I32_ne,      LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a /= b))
  -- (I32_lt_s,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (i32 a < i32 b))
  -- (I32_lt_u,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a < b))
  -- (I32_gt_s,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (i32 a > i32 b))
  -- (I32_gt_u,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a > b))
  -- (I32_le_s,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (i32 a <= i32 b))
  -- (I32_le_u,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a <= b))
  -- (I32_ge_s,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (i32 a >= i32 b))
  -- (I32_ge_u,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (bool (a >= b))
  -- (I64_eqz,     LitH (VI64 a) : args')                  -> LitH (bool (a == 0))
  -- (I64_eq,      LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a == b))
  -- (I64_ne,      LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a /= b))
  -- (I64_lt_s,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (i64 a < i64 b))
  -- (I64_lt_u,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a < b))
  -- (I64_gt_s,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (i64 a > i64 b))
  -- (I64_gt_u,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a > b))
  -- (I64_le_s,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (i64 a <= i64 b))
  -- (I64_le_u,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a <= b))
  -- (I64_ge_s,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (i64 a >= i64 b))
  -- (I64_ge_u,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (bool (a >= b))
  -- (F32_eq,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a == b))
  -- (F32_ne,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a /= b))
  -- (F32_lt,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a < b))
  -- (F32_gt,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a > b))
  -- (F32_le,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a <= b))
  -- (F32_ge,      LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (bool (a >= b))
  -- (F64_eq,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a == b))
  -- (F64_ne,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a /= b))
  -- (F64_lt,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a < b))
  -- (F64_gt,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a > b))
  -- (F64_le,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a <= b))
  -- (F64_ge,      LitH (VF64 a) : LitH (VF64 b) : args')  -> LitH (bool (a >= b))
  -- (I32_clz,     LitH (VI32 a) : args')                  -> LitH (VI32 $ cst32 $ countLeadingZeros a)
  -- (I32_ctz,     LitH (VI32 a) : args')                  -> LitH (VI32 $ cst32 $ countTrailingZeros a)
  -- (I32_popcnt,  LitH (VI32 a) : args')                  -> LitH (VI32 $ cst32 $ popCount a)
  -- (I32_add,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a + b))
  -- (I32_sub,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a - b))
  -- (I32_mul,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a * b))
  -- (I32_div_s,   LitH (VI32 a) : LitH (VI32 b) : args')  ->
  --     if (b == 0 || (a == 0x80000000 && b == 0xFFFFFFFF))
  --     then LitH (VException)
  --     else LitH (VI32 (u32 $ i32 a `quot` i32 b))
  -- (I32_div_u,   LitH (VI32 a) : LitH (VI32 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI32 (a `quot` b))
  -- (I32_rem_s,   LitH (VI32 a) : LitH (VI32 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI32 (u32 $ i32 a `rem` i32 b))
  -- (I32_rem_u,   LitH (VI32 a) : LitH (VI32 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI32 (a `rem` b))
  -- (I32_and,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a .&. b))
  -- (I32_or,      LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a .|. b))
  -- (I32_xor,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a `xor` b))
  -- (I32_shl,     LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a `shiftL` (fromIntegral b `rem` 32)))
  -- (I32_shr_u,   LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a `shiftR` (fromIntegral b `rem` 32)))
  -- (I32_shr_s,   LitH (VI32 a) : LitH (VI32 b) : args')  ->
  --     LitH (VI32 (u32 $ i32 a `shiftR` (fromIntegral b `rem` 32)))
  -- (I32_rotl,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a `rotateL` fromIntegral b))
  -- (I32_rotr,    LitH (VI32 a) : LitH (VI32 b) : args')  -> LitH (VI32 (a `rotateR` fromIntegral b))
  -- (I64_clz,     LitH (VI64 a) : args')                  -> LitH (VI64 $ cst64 $ countLeadingZeros a)
  -- (I64_ctz,     LitH (VI64 a) : args')                  -> LitH (VI64 $ cst64 $ countTrailingZeros a)
  -- (I64_popcnt,  LitH (VI64 a) : args')                  -> LitH (VI64 $ cst64 $ popCount a)
  -- (I64_add,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a + b))
  -- (I64_sub,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a - b))
  -- (I64_mul,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a * b))
  -- (I64_div_s,   LitH (VI64 a) : LitH (VI64 b) : args')  ->
  --     if (b == 0 || (a == 0x8000000000000000 && b == 0xFFFFFFFFFFFFFFFF))
  --     then LitH (VException)
  --     else LitH (VI64 (u64 $ i64 a `quot` i64 b))
  -- (I64_div_u,   LitH (VI64 a) : LitH (VI64 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI64 (a `quot` b))
  -- (I64_rem_s,   LitH (VI64 a) : LitH (VI64 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI64 (u64 $ i64 a `rem` i64 b))
  -- (I64_rem_u,   LitH (VI64 a) : LitH (VI64 b) : args')  ->
  --     if b == 0
  --     then LitH (VException)
  --     else LitH (VI64 (a `rem` b))
  -- (I64_and,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a .&. b))
  -- (I64_or,      LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a .|. b))
  -- (I64_xor,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a `xor` b))
  -- (I64_shl,     LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a `shiftL` (fromIntegral b `rem` 64)))
  -- (I64_shr_u,   LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a `shiftR` (fromIntegral b `rem` 64)))
  -- (I64_shr_s,   LitH (VI64 a) : LitH (VI64 b) : args')  ->
  --     LitH (VI64 (u64 $ i64 a `shiftR` (fromIntegral b `rem` 64)))
  -- (I64_rotl,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a `rotateL` fromIntegral b))
  -- (I64_rotr,    LitH (VI64 a) : LitH (VI64 b) : args')  -> LitH (VI64 (a `rotateR` fromIntegral b))
  -- (F32_abs,     LitH (VF32 a) : args')                  -> LitH (VF32 $ abs a)
  -- (F32_neg,     LitH (VF32 a) : args')                  -> LitH (VF32 $ negate a)
  -- (F32_ceil,    LitH (VF32 a) : args')                  -> LitH (VF32 $ floatCeil a)
  -- (F32_floor,   LitH (VF32 a) : args')                  -> LitH (VF32 $ floatFloor a)
  -- (F32_trunc,   LitH (VF32 a) : args')                  -> LitH (VF32 $ floatTrunc a)
  -- (F32_nearest, LitH (VF32 a) : args')                  -> LitH (VF32 $ nearest a)
  -- (F32_sqrt,    LitH (VF32 a) : args')                  -> LitH (VF32 $ sqrt a)
  -- (F32_add,     LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (VF32 (a + b))
  -- (F32_sub,     LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (VF32 (a - b))
  -- (F32_mul,     LitH (VF32 a) : LitH (VF32 b) : args')  -> LitH (VF32 (a * b))
  -- (F32_div,      LitH (VF32 a) : LitH (VF32 b) : args') -> LitH (VF32 (a / b))
  -- (F32_min,      LitH (VF32 a) : LitH (VF32 b) : args') -> LitH (VF32 (a `zeroAwareMin` b))
  -- (F32_max,      LitH (VF32 a) : LitH (VF32 b) : args') -> LitH (VF32 (a `zeroAwareMax` b))
  -- (F32_copysign, LitH (VF32 a) : LitH (VF32 b) : args') -> LitH (VF32 (a `copySign` b))
  -- (F64_abs,      LitH (VF64 a) : args')                 -> LitH (VF64 $ abs a)
  -- (F64_neg,      LitH (VF64 a) : args')                 -> LitH (VF64 $ negate a)
  -- (F64_ceil,     LitH (VF64 a) : args')                 -> LitH (VF64 $ doubleCeil a)
  -- (F64_floor,    LitH (VF64 a) : args')                 -> LitH (VF64 $ doubleFloor a)
  -- (F64_trunc,    LitH (VF64 a) : args')                 -> LitH (VF64 $ doubleTrunc a)
  -- (F64_nearest,  LitH (VF64 a) : args')                 -> LitH (VF64 $ nearest a)
  -- (F64_sqrt,     LitH (VF64 a) : args')                 -> LitH (VF64 $ sqrt a)
  -- (F64_add,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a + b))
  -- (F64_sub,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a - b))
  -- (F64_mul,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a * b))
  -- (F64_div,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a / b))
  -- (F64_min,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a `zeroAwareMin` b))
  -- (F64_max,      LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a `zeroAwareMax` b))
  -- (F64_copysign, LitH (VF64 a) : LitH (VF64 b) : args') -> LitH (VF64 (a `copySign` b))
  -- (I32_wrap_I64, LitH (VI64 a) : args')        ->
  --   LitH (VI32 $ (fromIntegral $ a .&. 0xFFFFFFFF))
  -- (I32_trunc_F32_s, LitH (VF32 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^31 || a < -2^31) then noredex
  --    else LitH (VI32 (u32 $ truncate a))
  -- (I32_trunc_F32_u, LitH (VF32 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^32 || a <= -1) 
  --    then LitH VException
  --    else LitH (VI32 (truncate a))
  -- (I32_trunc_F64_s, LitH (VF64 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^63 || a < -2^63)
  --    then LitH VException
  --    else LitH (VI32 (u32 $ truncate a))
  -- (I32_trunc_F64_u, LitH (VF64 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^64 || a <= -1) 
  --    then LitH VException
  --    else LitH (VI32 (truncate a))
  -- (I64_extend_I32_s, LitH (VI32 a) : args')    -> LitH (VI64 (u64 $ fromIntegral $ i32 a))
  -- (I64_extend_I32_u, LitH (VI32 a) : args')    -> LitH (VI64 (fromIntegral a))
  -- (I64_trunc_F32_s, LitH (VF32 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^31 || a < -2^31) 
  --    then LitH VException
  --    else LitH (VI64 (u64 $ truncate a))
  -- (I64_trunc_F32_u, LitH (VF32 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^32 || a <= -1) 
  --    then LitH VException
  --    else LitH (VI64 (truncate a))
  -- (I64_trunc_F64_s, LitH (VF64 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^63 || a < -2^63) 
  --    then noredex -- WasmTrap
  --    else LitH (VI64 (u64 $ truncate a))
  -- (I64_trunc_F64_u, LitH (VF64 a) : args')     ->
  --    if (isNaN a || isInfinite a || a >= 2^64 || a <= -1) 
  --    then LitH VException
  --    else LitH (VI64 (truncate a))
  -- (F32_convert_I32_s, LitH (VI32 a) : args')   -> LitH (VF32 (realToFrac $ i32 a))
  -- (F32_convert_I32_u, LitH (VI32 a) : args')   -> LitH (VF32 (realToFrac a))
  -- (F32_convert_I64_s, LitH (VI64 a) : args')   -> LitH (VF32 (realToFrac $ i64 a))
  -- (F32_convert_I64_u, LitH (VI64 a) : args')   -> LitH (VF32 (realToFrac a))
  -- (F32_demote_F64, LitH (VF64 a) : args')      -> LitH (VF32 (realToFrac a))
  -- (F64_convert_I32_s, LitH (VI32 a) : args')   -> LitH (VF64 (realToFrac $ i32 a))
  -- (F64_convert_I32_u, LitH (VI32 a) : args')   -> LitH (VF64 (realToFrac a))
  -- (F64_convert_I64_s, LitH (VI64 a) : args')   -> LitH (VF64 (realToFrac $ i64 a))
  -- (F64_convert_I64_u, LitH (VI64 a) : args')   -> LitH (VF64 (realToFrac a))
  -- (F64_promote_F32, LitH (VF32 a) : args')     -> LitH (VF64 (realToFrac a))
  -- (I32_reinterpret_F32, LitH (VF32 a) : args') -> LitH (VI32 (floatToWord a))
  -- (I64_reinterpret_F64, LitH (VF64 a) : args') -> LitH (VI64 (doubleToWord a))
  -- (F32_reinterpret_I32, LitH (VI32 a) : args') -> LitH (VF32 (wordToFloat a))
  -- (F64_reinterpret_I64, LitH (VI64 a) : args') -> LitH (VF64 (wordToDouble a))
  (Natural_succ,   LitH (VNatural n) : args')  -> LitH (VNatural $ n+1)
  (Natural_pred,   LitH (VNatural 0) : args')  -> LitH (VNatural 0)
  (Natural_pred,   LitH (VNatural n) : args')  -> LitH (VNatural $ n-1)
  (Natural_add,    LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (VNatural $ a + b)
  (Natural_sub,    LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (VNatural $ ite (a < b) 0 (a - b))
  (Natural_mul,    LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (VNatural $ a * b)
  (Natural_div,    LitH (VNatural a) : LitH (VNatural 0) : args') -> LitH (VNatural $ 0)
  (Natural_div,    LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (VNatural $ a `div` b)
  (Natural_mod,    LitH (VNatural a) : LitH (VNatural 0) : args') -> LitH (VNatural $ 0)
  (Natural_mod,    LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (VNatural $ a `mod` b)
  (Natural_gt,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a > b))
  (Natural_ge,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a >= b))
  (Natural_eq,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a == b))
  (Natural_ne,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a /= b))
  (Natural_lt,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a < b))
  (Natural_le,     LitH (VNatural a) : LitH (VNatural b) : args') -> LitH (bool (a <= b))
  (Natural_to_I64, LitH (VNatural a) : args') ->
    if a >= 2^64
    then LitH VException
    else LitH $ VI64 $ fromIntegral a
  (Natural_to_I32, LitH (VNatural a) : args') ->
    if a >= 2^32 
    then LitH VException
    else LitH $ VI32 $ fromIntegral a
  (Natural_from_I64, LitH (VI64 a) : args') -> LitH $ VNatural $ fromIntegral a
  (Natural_from_I32, LitH (VI32 a) : args') -> LitH $ VNatural $ fromIntegral a
  -- (BitVector_b0, LitH (VBitVector n bs) : args') -> LitH $ VBitVector (n+1) $
  --   case BS.uncons bs of
  --     Nothing       -> BS.singleton 0
  --     Just (255,cs) -> BS.cons 0 $ BS.cons 255 $ cs
  --     Just (c,cs)   -> bs
  -- (BitVector_b1, LitH (VBitVector n bs) : args') -> LitH $ VBitVector (n+1) $
  --     case BS.uncons bs of
  --       Nothing       -> BS.singleton 1
  --       Just (255,cs) -> BS.cons 1 $ BS.cons 0 $ cs
  --       Just (c,cs)   -> BS.cons (c+1) bs
  -- (BitVector_concat, LitH (VBitVector n a) : LitH (VBitVector m b) : args') -> LitH (VBitVector (n+m) $ a <> b)
  -- (BitVector_length, LitH (VBitVector n a) : args')                                 -> LitH (VNatural n)
  -- (String_cons, LitH (VChar c) : LitH (VString txt) : args')   -> LitH (VString (T.cons c txt))
  -- (String_concat, LitH (VString a) : LitH (VString b) : args') -> LitH (VString $ a <> b)
  -- (Char_ord, LitH (VChar c) : args')        -> LitH (VI64 $ fromIntegral $ ord c)
  -- (Char_chr, LitH (VI64 x) : args')         -> LitH (VChar $ chr $ fromIntegral x)
  _                                 -> noredex
  where
    apply []         trm = trm
    apply (arg:args) trm = apply args (AppH trm arg)
    noredex  = apply args (OprH op)
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

expandLit :: Literal -> Hoas
expandLit t = termToHoas Ctx.empty $ case t of
  VNatural 0 -> [yatima| λ P z s => z|]
  VNatural n -> App [yatima| λ x P z s => s x|] (Lit $ VNatural (n-1))
  VString cs -> case T.uncons cs of
    Nothing     -> [yatima| λ P n c => n|]
    Just (x,xs) -> let f = [yatima| λ x xs P n c => c x xs|]
                    in App (App f (Lit $ VChar x)) (Lit $ VString xs)
  VBitVector l bs -> case BS.uncons bs of
    Nothing -> [yatima| λ P be b0 b1 => be |]
    Just (0,xs) -> let f = [yatima| λ n xs P be b0 b1 => b0 n xs|]
                    in App (App f (Lit $ VNatural (l-1))) (Lit $ VBitVector (l-1) xs)
    Just (x,xs) ->
      if | x `mod` 2 == 0 ->
           let f = [yatima| λ n xs P be b0 b1 => b0 n xs|]
            in (App (App f (Lit $ VNatural (l-1))) (Lit $ VBitVector (l-1) (BS.cons (x `div` 2) xs)))
         | x `mod` 2 == 1 ->
           let f = [yatima| λ n xs P be b0 b1 => b1 n xs|]
            in (App (App f (Lit $ VNatural (l-1))) (Lit $ VBitVector (l-1) (BS.cons (x `div` 2) xs)))
  _        -> error "TODO"

litInduction :: LitType -> Hoas -> Hoas
litInduction t val = (\x y -> AppH y x) val $ termToHoas Ctx.empty $ case t of
  TNatural -> [yatima|
    λ self => forall
    (P : forall #Natural -> Type)
    (& zero : P 0)
    (& succ : forall (pred : #Natural) -> P (#Natural_succ pred))
    -> P self|]
  TString -> [yatima|
    λ self => forall
    (P : forall #String -> Type)
    (& nil  : P "")
    (& cons : forall (x: #Char) (xs : #String) -> P (#String_cons x xs))
    -> P self|]
  TBitVector -> [yatima|
    λ n self => forall
   (P    : forall (n: #Natural) (#BitVector n) -> Type)
   (& be : P 0 #b)
   (& b0 : forall (n: #Natural) (xs : #String)
     -> P (#Natural_succ n) (#BitVector_b0 n xs))
   (& b1 : forall (n: #Natural) (xs : #String)
     -> P (#Natural_succ n) (#BitVector_b1 n xs))
   -> P n self
  |]
  _    -> error "TODO"

typeOfLit :: Literal -> Hoas
typeOfLit t = case t of
  VWorld         -> LTyH TWorld
  VNatural _     -> LTyH TNatural
  VF64    _      -> LTyH TF64
  VF32    _      -> LTyH TF32
  VI64    _      -> LTyH TI64
  VI32    _      -> LTyH TI32
  VBitVector l _ -> (AppH (LTyH TBitVector) (LitH (VNatural l)))
  VString _      -> LTyH TString
  VChar _        -> LTyH TChar

typeOfLTy :: LitType -> Hoas
typeOfLTy t = case t of
  TBitVector -> AllH "" Many (LTyH TNatural) (\x -> TypH)
  _          -> TypH

typeOfOpr :: PrimOp -> Hoas
typeOfOpr t = termToHoas Ctx.empty $ case t of
  Natural_succ   -> [yatima|∀ #Natural -> #Natural|]
  Natural_pred   -> [yatima|∀ #Natural -> #Natural|]
  Natural_add    -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_sub    -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_div    -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_mod    -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_gt     -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_ge     -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_eq     -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_ne     -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_lt     -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_le     -> [yatima|∀ #Natural #Natural -> #I32|]
  String_cons    -> [yatima|∀ #Char #String -> #String|]
  String_concat  -> [yatima|∀ #String #String -> #String|]
  BitVector_b0   -> [yatima|∀ (n: #Natural) (#BitVector n) -> (#BitVector (#Natural_succ n))|]
  BitVector_b1   -> [yatima|∀ (n: #Natural) (#BitVector n) -> (#BitVector (#Natural_succ n))|]


