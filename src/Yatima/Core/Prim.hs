{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Yatima.Core.Prim
-- Description : Defines the semantics of Yatima's primitive literals and operations
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This module modifies work by [Ilya Rezvov](https://github.com/SPY/haskell-wasm),
-- which is released under MIT terms included with this package in the
-- @licenses/2018_Ilya_Rezvov@ file
module Yatima.Core.Prim where

import Data.Bits
import Data.Char
import qualified Data.Text as T
import Numeric.IEEE
import Yatima.Core.Hoas
import Yatima.Core.Wasm
import Yatima.QuasiQuoter
import Yatima.Term

oprArity :: PrimOp -> Int
oprArity opr = case opr of
  I32_eqz -> 1
  I32_clz -> 1
  I32_ctz -> 1
  I32_popcnt -> 1
  I64_clz -> 1
  I64_ctz -> 1
  I64_popcnt -> 1
  F32_abs -> 1
  F32_neg -> 1
  F32_ceil -> 1
  F32_floor -> 1
  F32_trunc -> 1
  F32_nearest -> 1
  F32_sqrt -> 1
  F64_abs -> 1
  F64_neg -> 1
  F64_ceil -> 1
  F64_floor -> 1
  F64_trunc -> 1
  F64_nearest -> 1
  F64_sqrt -> 1
  I32_wrap_I64 -> 1
  I32_trunc_F32_s -> 1
  I32_trunc_F32_u -> 1
  I32_trunc_F64_s -> 1
  I32_trunc_F64_u -> 1
  I64_extend_I32_s -> 1
  I64_extend_I32_u -> 1
  I64_trunc_F32_s -> 1
  I64_trunc_F32_u -> 1
  I64_trunc_F64_s -> 1
  I64_trunc_F64_u -> 1
  F32_convert_I32_s -> 1
  F32_convert_I32_u -> 1
  F32_convert_I64_s -> 1
  F32_convert_I64_u -> 1
  F32_demote_F64 -> 1
  F64_convert_I32_s -> 1
  F64_convert_I32_u -> 1
  F64_convert_I64_s -> 1
  F64_convert_I64_u -> 1
  F64_promote_F32 -> 1
  I32_reinterpret_F32 -> 1
  I64_reinterpret_F64 -> 1
  F32_reinterpret_I32 -> 1
  F64_reinterpret_I64 -> 1
  Natural_succ -> 1
  Natural_pred -> 1
  Natural_to_I64 -> 1
  Natural_to_I32 -> 1
  Natural_from_I64 -> 1
  Natural_from_I32 -> 1
  BitVector_b0 -> 1
  BitVector_b1 -> 1
  BitVector_length -> 1
  Char_chr -> 1
  Char_ord -> 1
  Char_to_U8 -> 1
  Char_from_U8 -> 1
  I32_to_U32 -> 1
  I32_from_U32 -> 1
  F32_to_U32 -> 1
  F32_from_U32 -> 1
  I64_to_U64 -> 1
  I64_from_U64 -> 1
  F64_to_U64 -> 1
  F64_from_U64 -> 1
  _ -> 2

inverseOpr :: PrimOp -> PrimOp -> Bool
inverseOpr op op' = case (op, op') of
  (Natural_pred, Natural_succ) -> True
  (I32_to_U32, I32_from_U32) -> True
  (I32_from_U32, I32_to_U32) -> True
  (F32_to_U32, I32_from_U32) -> True
  (F32_from_U32, I32_to_U32) -> True
  (I64_to_U64, I64_from_U64) -> True
  (I64_from_U64, I64_to_U64) -> True
  (F64_to_U64, F64_from_U64) -> True
  (F64_from_U64, F64_to_U64) -> True
  (Char_to_U8, Char_from_U8) -> True
  (Char_from_U8, Char_to_U8) -> True
  _ -> False

reduceOpr :: PrimOp -> [Hoas] -> Hoas
reduceOpr op args = apply rest $
  case operands of
    [AppH (OprH op') a] ->
      if inverseOpr op op' then a else noredex
    [AppH (OprH Natural_succ) a, b] ->
      case op of
        Natural_add -> AppH (OprH Natural_succ) (AppH (AppH (OprH Natural_add) a) b)
        _ -> noredex
    [LitH (VNatural a)] ->
      case op of
        Natural_succ -> LitH (VNatural $ a + 1)
        Natural_pred -> if a == 0 then LitH (VNatural 0) else LitH (VNatural $ a -1)
        Natural_to_I64 ->
          if a >= 2 ^ 64
            then termToHoas [] [yatima| #exception "Natural number out of range of I64"|]
            else LitH $ VI64 $ fromIntegral a
        Natural_to_I32 ->
          if a >= 2 ^ 32
            then termToHoas [] [yatima| #exception "Natural number out of range of I32"|]
            else LitH $ VI32 $ fromIntegral a
        Natural_from_I64 -> LitH $ VNatural $ fromIntegral a
        Natural_from_I32 -> LitH $ VNatural $ fromIntegral a
        _ -> noredex
    [LitH (VI64 a)] ->
      case op of
        I64_eqz -> LitH (bool (a == 0))
        I64_clz -> LitH (VI64 $ fromIntegral $ countLeadingZeros a)
        I64_ctz -> LitH (VI64 $ fromIntegral $ countTrailingZeros a)
        I64_popcnt -> LitH (VI64 $ fromIntegral $ popCount a)
        I32_wrap_I64 -> LitH (VI32 $ (fromIntegral $ a .&. 0xFFFFFFFF))
        F32_convert_I64_s -> LitH (VF32 (realToFrac $ i64 a))
        F32_convert_I64_u -> LitH (VF32 (realToFrac a))
        F64_reinterpret_I64 -> LitH (VF64 (wordToDouble a))
        F64_convert_I64_s -> LitH (VF64 (realToFrac $ i64 a))
        F64_convert_I64_u -> LitH (VF64 (realToFrac a))
        I64_to_U64 -> LitH (VBitVector 64 (convert a))
        Char_chr -> LitH (VChar $ chr $ fromIntegral a)
        _ -> noredex
    [LitH (VI32 a)] ->
      case op of
        I32_eqz -> LitH (bool (a == 0))
        I32_clz -> LitH (VI32 $ fromIntegral $ countLeadingZeros a)
        I32_ctz -> LitH (VI32 $ fromIntegral $ countTrailingZeros a)
        I32_popcnt -> LitH (VI32 $ fromIntegral $ popCount a)
        I64_extend_I32_s -> LitH (VI64 (u64 $ fromIntegral $ i32 a))
        I64_extend_I32_u -> LitH (VI64 (fromIntegral a))
        F32_reinterpret_I32 -> LitH (VF32 (wordToFloat a))
        F32_convert_I32_s -> LitH (VF32 (realToFrac $ i32 a))
        F32_convert_I32_u -> LitH (VF32 (realToFrac a))
        F64_convert_I32_s -> LitH (VF64 (realToFrac $ i32 a))
        F64_convert_I32_u -> LitH (VF64 (realToFrac a))
        I32_to_U32 -> LitH (VBitVector 32 (convert a))
        _ -> noredex
    [LitH (VF64 a)] ->
      case op of
        F64_abs -> LitH (VF64 $ abs a)
        F64_neg -> LitH (VF64 $ negate a)
        F64_ceil -> LitH (VF64 $ doubleCeil a)
        F64_floor -> LitH (VF64 $ doubleFloor a)
        F64_trunc -> LitH (VF64 $ doubleTrunc a)
        F64_nearest -> LitH (VF64 $ nearest a)
        F64_sqrt -> LitH (VF64 $ sqrt a)
        F64_promote_F32 -> LitH (VF64 (realToFrac a))
        I64_reinterpret_F64 -> LitH (VI64 (doubleToWord a))
        I32_trunc_F64_s ->
          if (isNaN a || isInfinite a || a >= 2 ^ 63 || a < -2 ^ 63)
            then termToHoas [] [yatima| #exception "F64 number out of range of I32"|]
            else LitH (VI32 (u32 $ truncate a))
        I32_trunc_F64_u ->
          if (isNaN a || isInfinite a || a >= 2 ^ 64 || a <= -1)
            then termToHoas [] [yatima| #exception "F64 number out of range of I32"|]
            else LitH (VI32 (truncate a))
        I64_trunc_F64_s ->
          if (isNaN a || isInfinite a || a >= 2 ^ 63 || a < -2 ^ 63)
            then termToHoas [] [yatima| #exception "F64 number out of range of I64" |]
            else LitH (VI64 (u64 $ truncate a))
        I64_trunc_F64_u ->
          if (isNaN a || isInfinite a || a >= 2 ^ 64 || a <= -1)
            then termToHoas [] [yatima| #exception "F64 number out of range of I64"|]
            else LitH (VI64 (truncate a))
        F64_to_U64 -> LitH (VBitVector 64 (convert $ doubleToWord a))
        _ -> noredex
    [LitH (VF32 a)] ->
      case op of
        F32_abs -> LitH (VF32 $ abs a)
        F32_neg -> LitH (VF32 $ negate a)
        F32_ceil -> LitH (VF32 $ floatCeil a)
        F32_floor -> LitH (VF32 $ floatFloor a)
        F32_trunc -> LitH (VF32 $ floatTrunc a)
        F32_nearest -> LitH (VF32 $ nearest a)
        F32_sqrt -> LitH (VF32 $ sqrt a)
        F32_demote_F64 -> LitH (VF32 (realToFrac a))
        I32_reinterpret_F32 -> LitH (VI32 (floatToWord a))
        I32_trunc_F32_s ->
          if (isNaN a || isInfinite a || a >= 2 ^ 31 || a < -2 ^ 31)
            then termToHoas [] [yatima| #exception "F32 number out of range of I32"|]
            else LitH (VI32 (u32 $ truncate a))
        I32_trunc_F32_u ->
          if (isNaN a || isInfinite a || a >= 2 ^ 32 || a <= -1)
            then termToHoas [] [yatima| #exception "F32 number out of range of I32"|]
            else LitH (VI32 (truncate a))
        I64_trunc_F32_s ->
          if (isNaN a || isInfinite a || a >= 2 ^ 31 || a < -2 ^ 31)
            then termToHoas [] [yatima| #exception "F32 number out of range of I64"|]
            else LitH (VI64 (u64 $ truncate a))
        I64_trunc_F32_u ->
          if (isNaN a || isInfinite a || a >= 2 ^ 32 || a <= -1)
            then termToHoas [] [yatima| #exception "F32 number out of range of I64"|]
            else LitH (VI64 (truncate a))
        F32_to_U32 -> LitH (VBitVector 32 (convert $ floatToWord a))
        _ -> noredex
    [LitH (VBitVector n bs)] ->
      case op of
        BitVector_b0 -> LitH $ VBitVector (n + 1) (shiftL bs 1)
        BitVector_b1 -> LitH $ VBitVector (n + 1) (shiftL bs 1 `xor` 1)
        BitVector_length -> LitH (VNatural n)
        Char_from_U8 ->
          if n == 8
            then LitH $ VChar $ convert bs
            else noredex
        I32_from_U32 ->
          if n == 32
            then LitH $ VI32 $ convert bs
            else noredex
        F32_from_U32 ->
          if n == 32
            then LitH $ VF32 $ wordToFloat $ convert bs
            else noredex
        I64_from_U64 ->
          if n == 64
            then LitH $ VI64 $ convert bs
            else noredex
        F64_from_U64 ->
          if n == 64
            then LitH $ VF64 $ wordToDouble $ convert bs
            else noredex
        _ -> noredex
    [LitH (VChar c)] ->
      case op of
        Char_to_U8 -> LitH (VBitVector 8 $ convert c)
        Char_ord -> LitH (VI64 $ fromIntegral $ ord c)
        _ -> noredex
    [LitH (VNatural a), LitH (VNatural b)] ->
      case op of
        Natural_add -> LitH (VNatural $ a + b)
        Natural_sub -> LitH (VNatural $ ite (a < b) 0 (a - b))
        Natural_mul -> LitH (VNatural $ a * b)
        Natural_div -> if b == 0 then LitH (VNatural $ 0) else LitH (VNatural $ a `div` b)
        Natural_mod -> if b == 0 then LitH (VNatural $ 0) else LitH (VNatural $ a `mod` b)
        Natural_gt -> LitH (bool (a > b))
        Natural_ge -> LitH (bool (a >= b))
        Natural_eq -> LitH (bool (a == b))
        Natural_ne -> LitH (bool (a /= b))
        Natural_lt -> LitH (bool (a < b))
        Natural_le -> LitH (bool (a <= b))
        _ -> noredex
    [LitH (VI64 a), LitH (VI64 b)] ->
      case op of
        I64_eq -> LitH (bool (a == b))
        I64_ne -> LitH (bool (a /= b))
        I64_lt_s -> LitH (bool (i64 a < i64 b))
        I64_lt_u -> LitH (bool (a < b))
        I64_gt_s -> LitH (bool (i64 a > i64 b))
        I64_gt_u -> LitH (bool (a > b))
        I64_le_s -> LitH (bool (i64 a <= i64 b))
        I64_le_u -> LitH (bool (a <= b))
        I64_ge_s -> LitH (bool (i64 a >= i64 b))
        I64_ge_u -> LitH (bool (a >= b))
        I64_add -> LitH (VI64 (a + b))
        I64_sub -> LitH (VI64 (a - b))
        I64_mul -> LitH (VI64 (a * b))
        I64_div_s ->
          if (b == 0 || (a == 0x8000000000000000 && b == 0xFFFFFFFFFFFFFFFF))
            then termToHoas [] [yatima| #exception "Cannot divide by zero"|]
            else LitH (VI64 (u64 $ i64 a `quot` i64 b))
        I64_div_u ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot divide by zero" |]
            else LitH (VI64 (a `quot` b))
        I64_rem_s ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot take the remainder of zero" |]
            else LitH (VI64 (u64 $ i64 a `rem` i64 b))
        I64_rem_u ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot take the remainder of zero" |]
            else LitH (VI64 (a `rem` b))
        I64_and -> LitH (VI64 (a .&. b))
        I64_or -> LitH (VI64 (a .|. b))
        I64_xor -> LitH (VI64 (a `xor` b))
        I64_shl -> LitH (VI64 (a `shiftL` (fromIntegral b `rem` 64)))
        I64_shr_u -> LitH (VI64 (a `shiftR` (fromIntegral b `rem` 64)))
        I64_shr_s -> LitH (VI64 (u64 $ i64 a `shiftR` (fromIntegral b `rem` 64)))
        I64_rotl -> LitH (VI64 (a `rotateL` fromIntegral b))
        I64_rotr -> LitH (VI64 (a `rotateR` fromIntegral b))
        _ -> noredex
    [LitH (VI32 a), LitH (VI32 b)] ->
      case op of
        I32_eq -> LitH (bool (a == b))
        I32_ne -> LitH (bool (a /= b))
        I32_lt_s -> LitH (bool (i32 a < i32 b))
        I32_lt_u -> LitH (bool (a < b))
        I32_gt_s -> LitH (bool (i32 a > i32 b))
        I32_gt_u -> LitH (bool (a > b))
        I32_le_s -> LitH (bool (i32 a <= i32 b))
        I32_le_u -> LitH (bool (a <= b))
        I32_ge_s -> LitH (bool (i32 a >= i32 b))
        I32_ge_u -> LitH (bool (a >= b))
        I32_add -> LitH (VI32 (a + b))
        I32_sub -> LitH (VI32 (a - b))
        I32_mul -> LitH (VI32 (a * b))
        I32_div_s ->
          if (b == 0 || (a == 0x80000000 && b == 0xFFFFFFFF))
            then termToHoas [] [yatima| #exception "Cannot divide by zero" |]
            else LitH (VI32 (u32 $ i32 a `quot` i32 b))
        I32_div_u ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot divide by zero" |]
            else LitH (VI32 (a `quot` b))
        I32_rem_s ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot take the remainder of zero" |]
            else LitH (VI32 (u32 $ i32 a `rem` i32 b))
        I32_rem_u ->
          if b == 0
            then termToHoas [] [yatima| #exception "Cannot take the remainder of zero" |]
            else LitH (VI32 (a `rem` b))
        I32_and -> LitH (VI32 (a .&. b))
        I32_or -> LitH (VI32 (a .|. b))
        I32_xor -> LitH (VI32 (a `xor` b))
        I32_shl -> LitH (VI32 (a `shiftL` (fromIntegral b `rem` 32)))
        I32_shr_u -> LitH (VI32 (a `shiftR` (fromIntegral b `rem` 32)))
        I32_shr_s -> LitH (VI32 (u32 $ i32 a `shiftR` (fromIntegral b `rem` 32)))
        I32_rotl -> LitH (VI32 (a `rotateL` fromIntegral b))
        I32_rotr -> LitH (VI32 (a `rotateR` fromIntegral b))
        _ -> noredex
    [LitH (VF64 a), LitH (VF64 b)] ->
      case op of
        F64_eq -> LitH (bool (a == b))
        F64_ne -> LitH (bool (a /= b))
        F64_lt -> LitH (bool (a < b))
        F64_gt -> LitH (bool (a > b))
        F64_le -> LitH (bool (a <= b))
        F64_ge -> LitH (bool (a >= b))
        F64_add -> LitH (VF64 (a + b))
        F64_sub -> LitH (VF64 (a - b))
        F64_mul -> LitH (VF64 (a * b))
        F64_div -> LitH (VF64 (a / b))
        F64_min -> LitH (VF64 (a `zeroAwareMin` b))
        F64_max -> LitH (VF64 (a `zeroAwareMax` b))
        F64_copysign -> LitH (VF64 (a `copySign` b))
        _ -> noredex
    [LitH (VF32 a), LitH (VF32 b)] ->
      case op of
        F32_eq -> LitH (bool (a == b))
        F32_ne -> LitH (bool (a /= b))
        F32_lt -> LitH (bool (a < b))
        F32_gt -> LitH (bool (a > b))
        F32_le -> LitH (bool (a <= b))
        F32_ge -> LitH (bool (a >= b))
        F32_add -> LitH (VF32 (a + b))
        F32_sub -> LitH (VF32 (a - b))
        F32_mul -> LitH (VF32 (a * b))
        F32_div -> LitH (VF32 (a / b))
        F32_min -> LitH (VF32 (a `zeroAwareMin` b))
        F32_max -> LitH (VF32 (a `zeroAwareMax` b))
        F32_copysign -> LitH (VF32 (a `copySign` b))
        _ -> noredex
    [LitH (VBitVector n a), LitH (VBitVector m b)] ->
      case op of
        BitVector_concat -> LitH (VBitVector (n + m) $ a `xor` shiftL b (fromEnum m))
        _ -> noredex
    [LitH (VChar c), LitH (VString txt)] ->
      case op of
        String_cons -> LitH (VString (T.cons c txt))
        _ -> noredex
    [LitH (VString a), LitH (VString b)] ->
      case op of
        String_concat -> LitH (VString $ a <> b)
        _ -> noredex
    _ -> noredex
  where
    arity = oprArity op
    (operands, rest) = splitAt arity args
    apply args trm = foldl AppH trm args
    noredex = apply operands (OprH op)
    ite c t f = if c then t else f
    bool c = ite c (VI32 1) (VI32 0)
    i32 = asInt32
    i64 = asInt64
    u32 = asWord32
    u64 = asWord64
    convert :: (Enum a, Enum b) => a -> b
    convert = toEnum . fromEnum

expandLit :: Literal -> Maybe Hoas
expandLit t =
  termToHoas [] <$> case t of
    VNatural 0 -> Just ([yatima| λ P z s => z|])
    VNatural n -> Just $ App ([yatima| λ x P z s => s x|]) (Lit $ VNatural (n -1))
    VString cs -> Just $ case T.uncons cs of
      Nothing -> ([yatima| λ P n c => n|])
      Just (x, xs) ->
        let f = ([yatima| λ x xs P n c => c x xs|])
         in App (App f (Lit $ VChar x)) (Lit $ VString xs)
    VBitVector 0 _ -> Just ([yatima| λ P be b0 b1 => be |])
    VBitVector l bs ->
      let f =
            if bs .&. 1 == 1
              then ([yatima| λ n xs P be b0 b1 => b1 n xs|])
              else ([yatima| λ n xs P be b0 b1 => b0 n xs|])
       in Just $ App (App f (Lit $ VNatural (l -1))) (Lit $ VBitVector (l -1) (bs `shiftR` 1))
    _ -> Nothing

litInduction :: LitType -> Hoas -> Hoas
litInduction t val = (\y -> AppH y val) $
  termToHoas [] $ case t of
    TNatural ->
      [yatima|
    λ self => forall
    (0 P : forall #Natural -> Type)
    (& zero : P 0)
    (& succ : forall (pred : #Natural) -> P (#Natural.succ pred))
    -> P self|]
    TString ->
      [yatima|
    λ self => forall
    (0 P : forall #String -> Type)
    (& nil  : P "")
    (& cons : forall (x: #Char) (xs : #String) -> P (#String.cons x xs))
    -> P self|]
    TBitVector ->
      [yatima|
    λ n self => forall
    (0 P    : forall (n: #Natural) (#BitVector n) -> Type)
    (& be : P 0 #b)
    (& b0 : forall (n: #Natural) (xs : #BitVector n)
     -> P (#Natural.succ n) (#BitVector.b0 n xs))
    (& b1 : forall (n: #Natural) (xs : #BitVector n)
     -> P (#Natural.succ n) (#BitVector.b1 n xs))
    -> P n self
    |]
    _ -> error "Non-inductive type"

typeOfLit :: Literal -> Hoas
typeOfLit t = case t of
  VWorld -> LTyH TWorld
  VNatural _ -> LTyH TNatural
  VF64 _ -> LTyH TF64
  VF32 _ -> LTyH TF32
  VI64 _ -> LTyH TI64
  VI32 _ -> LTyH TI32
  VBitVector l _ -> (AppH (LTyH TBitVector) (LitH (VNatural l)))
  VString _ -> LTyH TString
  VChar _ -> LTyH TChar
  VException -> termToHoas [] [yatima| ∀ #String -> #Exception |]

typeOfLTy :: LitType -> Hoas
typeOfLTy t = case t of
  TBitVector -> termToHoas [] [yatima| ∀ #Natural -> Type|]
  _ -> TypH

typeOfOpr :: PrimOp -> Hoas
typeOfOpr t = termToHoas [] $ case t of
  Natural_from_I64 -> [yatima|∀ #I64 -> #Natural|]
  Natural_from_I32 -> [yatima|∀ #I32 -> #Natural|]
  Natural_succ -> [yatima|∀ #Natural -> #Natural|]
  Natural_pred -> [yatima|∀ #Natural -> #Natural|]
  Natural_add -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_sub -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_mul -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_gt -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_ge -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_eq -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_ne -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_lt -> [yatima|∀ #Natural #Natural -> #I32|]
  Natural_le -> [yatima|∀ #Natural #Natural -> #I32|]
  I64_eqz -> [yatima|∀ #I64 -> #I32|]
  I64_clz -> [yatima|∀ #I64 -> #I64|]
  I64_ctz -> [yatima|∀ #I64 -> #I64|]
  I64_popcnt -> [yatima|∀ #I64 -> #I64|]
  I32_wrap_I64 -> [yatima|∀ #I64 -> #I32|]
  F32_convert_I64_s -> [yatima|∀ #I64 -> #F32|]
  F32_convert_I64_u -> [yatima|∀ #I64 -> #F32|]
  F64_reinterpret_I64 -> [yatima|∀ #I64 -> #F64|]
  F64_convert_I64_s -> [yatima|∀ #I64 -> #F64|]
  F64_convert_I64_u -> [yatima|∀ #I64 -> #F64|]
  I64_to_U64 -> [yatima|∀ #I64 -> (#BitVector 64)|]
  Char_chr -> [yatima|∀ #I64 -> #Char|]
  I32_eqz -> [yatima|∀ #I32 -> #I32|]
  I32_clz -> [yatima|∀ #I32 -> #I32|]
  I32_ctz -> [yatima|∀ #I32 -> #I32|]
  I32_popcnt -> [yatima|∀ #I32 -> #I32|]
  I64_extend_I32_s -> [yatima|∀ #I32 -> #I64|]
  I64_extend_I32_u -> [yatima|∀ #I32 -> #I64|]
  F32_reinterpret_I32 -> [yatima|∀ #I32 -> #F32|]
  F32_convert_I32_s -> [yatima|∀ #I32 -> #F32|]
  F32_convert_I32_u -> [yatima|∀ #I32 -> #F64|]
  F64_convert_I32_s -> [yatima|∀ #I32 -> #F64|]
  F64_convert_I32_u -> [yatima|∀ #I32 -> #F64|]
  I32_to_U32 -> [yatima|∀ #I32 -> (#BitVector 32)|]
  F64_abs -> [yatima|∀ #F64 -> #F64|]
  F64_neg -> [yatima|∀ #F64 -> #F64|]
  F64_ceil -> [yatima|∀ #F64 -> #F64|]
  F64_floor -> [yatima|∀ #F64 -> #F64|]
  F64_trunc -> [yatima|∀ #F64 -> #F64|]
  F64_nearest -> [yatima|∀ #F64 -> #F64|]
  F64_sqrt -> [yatima|∀ #F64 -> #F64|]
  F64_promote_F32 -> [yatima|∀ #F64 -> #F64|]
  I64_reinterpret_F64 -> [yatima|∀ #F64 -> #I64|]
  F64_to_U64 -> [yatima|∀ #F64 -> (#BitVector 64)|]
  F32_abs -> [yatima|∀ #F32 -> #F32|]
  F32_neg -> [yatima|∀ #F32 -> #F32|]
  F32_ceil -> [yatima|∀ #F32 -> #F32|]
  F32_floor -> [yatima|∀ #F32 -> #F32|]
  F32_trunc -> [yatima|∀ #F32 -> #F32|]
  F32_nearest -> [yatima|∀ #F32 -> #F32|]
  F32_sqrt -> [yatima|∀ #F32 -> #F32|]
  F32_demote_F64 -> [yatima|∀ #F32 -> #F32|]
  I32_reinterpret_F32 -> [yatima|∀ #F32 -> #I32|]
  F32_to_U32 -> [yatima|∀ #F32 -> (#BitVector 32)|]
  String_cons -> [yatima|∀ #Char #String -> #String|]
  String_concat -> [yatima|∀ #String #String -> #String|]
  BitVector_b0 -> [yatima|∀ (0 n: #Natural) (#BitVector n) -> (#BitVector (#Natural.succ n))|]
  BitVector_b1 -> [yatima|∀ (0 n: #Natural) (#BitVector n) -> (#BitVector (#Natural.succ n))|]
  BitVector_length -> [yatima|∀ (0 n: #Natural) (#BitVector n) -> #Natural|]
  BitVector_concat ->
    [yatima|∀ (0 n: #Natural) (0 m: #Natural) (#BitVector n) (#BitVector m) -> (#BitVector (#Natural.add n m))|]
  Char_from_U8 -> [yatima|∀ (#BitVector 8) -> #Natural|]
  I32_from_U32 -> [yatima|∀ (#BitVector 32) -> #Natural|]
  F32_from_U32 -> [yatima|∀ (#BitVector 32) -> #Natural|]
  I64_from_U64 -> [yatima|∀ (#BitVector 64) -> #Natural|]
  F64_from_U64 -> [yatima|∀ (#BitVector 64) -> #Natural|]
  Char_to_U8 -> [yatima|∀ #Char -> (#BitVector 8)|]
  Char_ord -> [yatima|∀ #Char -> #I64|]
  I64_eq -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_ne -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_lt_s -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_lt_u -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_gt_s -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_gt_u -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_le_s -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_le_u -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_ge_s -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_ge_u -> [yatima|∀ #I64 #I64 -> #I32|]
  I64_add -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_sub -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_mul -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_and -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_or -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_xor -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_shl -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_shr_u -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_shr_s -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_rotl -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_rotr -> [yatima|∀ #I64 #I64 -> #I64|]
  I32_eq -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_ne -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_lt_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_lt_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_gt_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_gt_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_le_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_le_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_ge_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_ge_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_add -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_sub -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_mul -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_and -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_or -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_xor -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_shl -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_shr_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_shr_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_rotl -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_rotr -> [yatima|∀ #I32 #I32 -> #I32|]
  F64_eq -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_ne -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_lt -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_gt -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_le -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_ge -> [yatima|∀ #F64 #F64 -> #I32|]
  F64_add -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_sub -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_mul -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_div -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_min -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_max -> [yatima|∀ #F64 #F64 -> #F64|]
  F64_copysign -> [yatima|∀ #F64 #F64 -> #F64|]
  F32_eq -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_ne -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_lt -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_gt -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_le -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_ge -> [yatima|∀ #F32 #F32 -> #I32|]
  F32_add -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_sub -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_mul -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_div -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_min -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_max -> [yatima|∀ #F32 #F32 -> #F32|]
  F32_copysign -> [yatima|∀ #F32 #F32 -> #F32|]
  -- These ones might raise exceptions.
  Natural_to_I64 -> [yatima|∀ #Natural -> #I64|]
  Natural_to_I32 -> [yatima|∀ #Natural -> #I32|]
  Natural_div -> [yatima|∀ #Natural #Natural -> #Natural|]
  Natural_mod -> [yatima|∀ #Natural #Natural -> #Natural|]
  I32_trunc_F64_s -> [yatima|∀ #F64 -> #I32|]
  I32_trunc_F64_u -> [yatima|∀ #F64 -> #I32|]
  I64_trunc_F64_s -> [yatima|∀ #F64 -> #I64|]
  I64_trunc_F64_u -> [yatima|∀ #F64 -> #I64|]
  I32_trunc_F32_s -> [yatima|∀ #F32 -> #I32|]
  I32_trunc_F32_u -> [yatima|∀ #F32 -> #I32|]
  I64_trunc_F32_s -> [yatima|∀ #F32 -> #I64|]
  I64_trunc_F32_u -> [yatima|∀ #F32 -> #I64|]
  I64_div_s -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_div_u -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_rem_s -> [yatima|∀ #I64 #I64 -> #I64|]
  I64_rem_u -> [yatima|∀ #I64 #I64 -> #I64|]
  I32_div_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_div_u -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_rem_s -> [yatima|∀ #I32 #I32 -> #I32|]
  I32_rem_u -> [yatima|∀ #I32 #I32 -> #I32|]
