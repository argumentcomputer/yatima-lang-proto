{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Core where

import Control.Monad.Except
import Data.Int
import qualified Data.Map as M
import Data.Word
import Test.Hspec
import Test.QuickCheck
import qualified Yatima.Core as Core
import Yatima.Core.CheckError
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Core.Hoas
import Yatima.Core.Wasm
import Yatima.QuasiQuoter
import Yatima.Term

bool :: Bool -> Literal
bool c = if c then (VI32 1) else (VI32 0)

whnf :: Term -> Term
whnf t = hoasToTerm 0 $ Core.whnf M.empty (FixH "test" (\s -> (termToHoas [s] t)))

check :: Term -> Term -> Either CheckError ()
check trm typ =
  let x = Core.check M.empty Ctx.empty Once (termToHoas [] trm) (termToHoas [] typ)
   in case runExcept x of
        Left e -> Left e
        Right _ -> Right ()

rel_I32_prop :: PrimOp -> (Word32 -> Word32 -> Bool) -> (Word32, Word32) -> Bool
rel_I32_prop op f (x, y) =
  whnf (App (App (Opr op) (Lit $ VI32 x)) (Lit $ VI32 y)) == (Lit (bool $ f x y))

signed32_op :: (Int32 -> Int32 -> Bool) -> Word32 -> Word32 -> Bool
signed32_op f x y = f (asInt32 x) (asInt32 y)

rel_I64_prop :: PrimOp -> (Word64 -> Word64 -> Bool) -> (Word64, Word64) -> Bool
rel_I64_prop op f (x, y) =
  whnf (App (App (Opr op) (Lit $ VI64 x)) (Lit $ VI64 y)) == (Lit (bool $ f x y))

signed64_op :: (Int64 -> Int64 -> Bool) -> Word64 -> Word64 -> Bool
signed64_op f x y = f (asInt64 x) (asInt64 y)

eval_spec :: Spec
eval_spec = do
  describe "`whnf` evaluation " $ do
    it "" $ whnf [yatima| (λ x => x) 1 |] `shouldBe` [yatima| 1 |]
    it "" $ whnf [yatima| (λ x y => x) 1 2 |] `shouldBe` [yatima| 1 |]
    it "" $ whnf [yatima| (λ x y => y) 1 2 |] `shouldBe` [yatima| 2 |]
    it "" $ whnf [yatima| (λ x y => y) 1 |] `shouldBe` [yatima| λ y => y |]
    it "" $ whnf [yatima| (λ x y => x) (λ x => x) |] `shouldBe` [yatima| λ y => (λ x => x) |]
  describe "`whnf` evaluation of Naturals " $ do
    it "" $ whnf [yatima| #Natural_succ 1 |] `shouldBe` [yatima| 2 |]
    it "" $ whnf [yatima| (case 0)|] `shouldBe` [yatima| λ P z s => z |]
    it "" $ whnf [yatima| (case 1)|] `shouldBe` [yatima| λ P z s => s 0 |]
    it "" $ whnf [yatima| (case 2)|] `shouldBe` [yatima| λ P z s => s 1 |]
  describe "`whnf` evaluation of I32 operations" $ do
    it "I32_eq" $ property $ rel_I32_prop I32_eq (==)
    it "I32_ne" $ property $ rel_I32_prop I32_ne (/=)
    it "I32_lt_u" $ property $ rel_I32_prop I32_lt_u (<)
    it "I32_le_u" $ property $ rel_I32_prop I32_le_u (<=)
    it "I32_gt_u" $ property $ rel_I32_prop I32_gt_u (>)
    it "I32_ge_u" $ property $ rel_I32_prop I32_ge_u (>=)
    it "I32_lt_s" $ property $ rel_I32_prop I32_lt_s (signed32_op (<))
    it "I32_le_s" $ property $ rel_I32_prop I32_le_s (signed32_op (<=))
    it "I32_gt_s" $ property $ rel_I32_prop I32_gt_s (signed32_op (>))
    it "I32_ge_s" $ property $ rel_I32_prop I32_ge_s (signed32_op (>=))
    it "" $ whnf [yatima| #I32_add 1u32 1u32|] `shouldBe` [yatima| 2u32 |]
    it "" $ whnf [yatima| #I32_add 1i32 -1i32|] `shouldBe` [yatima| 0i32 |]
    it "" $ whnf [yatima| #I32_sub 1u32 1u32|] `shouldBe` [yatima| 0i32 |]
    it "" $ whnf [yatima| #I32_sub 1i32 -1i32|] `shouldBe` [yatima| 2i32 |]
    it "" $ whnf [yatima| #I32_div_s 1i32 0i32|] `shouldBe` [yatima| #exception |]
    it "" $ whnf [yatima| #I32_and 0b1111u32 0b1111u32|] `shouldBe` [yatima| 0b1111u32 |]
    it "" $ whnf [yatima| #I32_xor 0b1111u32 0b1111u32|] `shouldBe` [yatima| 0b0000u32 |]
    it "" $ whnf [yatima| #I32_shl 0b1u32 3u32 |] `shouldBe` [yatima| 0b1000u32 |]
    it "" $ whnf [yatima| #I32_shr_u 0b1000u32 3u32 |] `shouldBe` [yatima| 0b1u32 |]
  -- todo: mul, rem, mod, or, shr_s, rotl, rotr
  describe "`whnf` evaluation of F32 operations" $ do
    it "" $ whnf [yatima| #F32_eq 1.0f32 2.0f32 |] `shouldBe` [yatima| 0u32 |]
    it "" $ whnf [yatima| #F32_mul 2.0f32 2.0f32 |] `shouldBe` [yatima| 4.0f32 |]
  describe "`whnf` evaluation of I64 operations" $ do
    it "I64_eq" $ property $ rel_I64_prop I64_eq (==)
    it "I64_ne" $ property $ rel_I64_prop I64_ne (/=)
    it "I64_lt_u" $ property $ rel_I64_prop I64_lt_u (<)
    it "I64_le_u" $ property $ rel_I64_prop I64_le_u (<=)
    it "I64_gt_u" $ property $ rel_I64_prop I64_gt_u (>)
    it "I64_ge_u" $ property $ rel_I64_prop I64_ge_u (>=)
    it "I64_lt_s" $ property $ rel_I64_prop I64_lt_s (signed64_op (<))
    it "I64_le_s" $ property $ rel_I64_prop I64_le_s (signed64_op (<=))
    it "I64_gt_s" $ property $ rel_I64_prop I64_gt_s (signed64_op (>))
    it "I64_ge_s" $ property $ rel_I64_prop I64_ge_s (signed64_op (>=))
  describe "`whnf` evaluation of String operations" $ do
    it "" $ whnf [yatima| #String_cons 'f' "oo" |] `shouldBe` [yatima| "foo" |]
    it "" $ whnf [yatima| #String_concat "foo" "bar" |] `shouldBe` [yatima| "foobar" |]
  describe "`whnf` evaluation of BitVector operations" $ do
    it "" $ whnf [yatima| #BitVector_concat #xff #xff |] `shouldBe` [yatima| #xffff |]
