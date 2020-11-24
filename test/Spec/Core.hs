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
whnf t = hoasToTerm 0 $ Core.whnf M.empty (FixH NoLoc "test" (\s -> (termToHoas [s] t)))

check :: Term -> Term -> Either CheckError ()
check trm typ =
  let x = Core.check Nothing M.empty Ctx.empty Once (termToHoas [] trm) (termToHoas [] typ)
   in case runExcept x of
        Left e -> Left e
        Right _ -> Right ()

rel_I32_prop :: PrimOp -> (Word32 -> Word32 -> Bool) -> (Word32, Word32) -> Bool
rel_I32_prop op f (x, y) =
  whnf (_App (_App (_Opr op) (_Lit $ VI32 x)) (_Lit $ VI32 y)) == (_Lit (bool $ f x y))

signed32_op :: (Int32 -> Int32 -> Bool) -> Word32 -> Word32 -> Bool
signed32_op f x y = f (asInt32 x) (asInt32 y)

rel_I64_prop :: PrimOp -> (Word64 -> Word64 -> Bool) -> (Word64, Word64) -> Bool
rel_I64_prop op f (x, y) =
  whnf (_App (_App (_Opr op) (_Lit $ VI64 x)) (_Lit $ VI64 y)) == (_Lit (bool $ f x y))

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
    it "" $ whnf [yatima| #Natural.succ 1 |] `shouldBe` [yatima| 2 |]
    it "" $ whnf [yatima| (case 0)|] `shouldBe` [yatima| λ P z s => z |]
    it "" $ whnf [yatima| (case 1)|] `shouldBe` [yatima| λ P z s => s 0 |]
    it "" $ whnf [yatima| (case 2)|] `shouldBe` [yatima| λ P z s => s 1 |]
  describe "`whnf` evaluation of I32 operations" $ do
    it "I32.eq" $ property $ rel_I32_prop I32_eq (==)
    it "I32.ne" $ property $ rel_I32_prop I32_ne (/=)
    it "I32.lt_u" $ property $ rel_I32_prop I32_lt_u (<)
    it "I32.le_u" $ property $ rel_I32_prop I32_le_u (<=)
    it "I32.gt_u" $ property $ rel_I32_prop I32_gt_u (>)
    it "I32.ge_u" $ property $ rel_I32_prop I32_ge_u (>=)
    it "I32.lt_s" $ property $ rel_I32_prop I32_lt_s (signed32_op (<))
    it "I32.le_s" $ property $ rel_I32_prop I32_le_s (signed32_op (<=))
    it "I32.gt_s" $ property $ rel_I32_prop I32_gt_s (signed32_op (>))
    it "I32.ge_s" $ property $ rel_I32_prop I32_ge_s (signed32_op (>=))
    it "" $ whnf [yatima| #I32.add 1u32 1u32|] `shouldBe` [yatima| 2u32 |]
    it "" $ whnf [yatima| #I32.add 1i32 -1i32|] `shouldBe` [yatima| 0i32 |]
    it "" $ whnf [yatima| #I32.sub 1u32 1u32|] `shouldBe` [yatima| 0i32 |]
    it "" $ whnf [yatima| #I32.sub 1i32 -1i32|] `shouldBe` [yatima| 2i32 |]
    it "" $ whnf [yatima| #I32.div_s 1i32 0i32|] `shouldBe` [yatima| #exception "Cannot divide by zero" |]
    it "" $ whnf [yatima| #I32.and 0b1111u32 0b1111u32|] `shouldBe` [yatima| 0b1111u32 |]
    it "" $ whnf [yatima| #I32.xor 0b1111u32 0b1111u32|] `shouldBe` [yatima| 0b0000u32 |]
    it "" $ whnf [yatima| #I32.shl 0b1u32 3u32 |] `shouldBe` [yatima| 0b1000u32 |]
    it "" $ whnf [yatima| #I32.shr_u 0b1000u32 3u32 |] `shouldBe` [yatima| 0b1u32 |]
  -- todo: mul, rem, mod, or, shr_s, rotl, rotr
  describe "`whnf` evaluation of F32 operations" $ do
    it "" $ whnf [yatima| #F32.eq 1.0f32 2.0f32 |] `shouldBe` [yatima| 0u32 |]
    it "" $ whnf [yatima| #F32.mul 2.0f32 2.0f32 |] `shouldBe` [yatima| 4.0f32 |]
  describe "`whnf` evaluation of I64 operations" $ do
    it "I64.eq" $ property $ rel_I64_prop I64_eq (==)
    it "I64.ne" $ property $ rel_I64_prop I64_ne (/=)
    it "I64.lt_u" $ property $ rel_I64_prop I64_lt_u (<)
    it "I64.le_u" $ property $ rel_I64_prop I64_le_u (<=)
    it "I64.gt_u" $ property $ rel_I64_prop I64_gt_u (>)
    it "I64.ge_u" $ property $ rel_I64_prop I64_ge_u (>=)
    it "I64.lt_s" $ property $ rel_I64_prop I64_lt_s (signed64_op (<))
    it "I64.le_s" $ property $ rel_I64_prop I64_le_s (signed64_op (<=))
    it "I64.gt_s" $ property $ rel_I64_prop I64_gt_s (signed64_op (>))
    it "I64.ge_s" $ property $ rel_I64_prop I64_ge_s (signed64_op (>=))
  describe "`whnf` evaluation of String operations" $ do
    it "" $ whnf [yatima| #String.cons 'f' "oo" |] `shouldBe` [yatima| "foo" |]
    it "" $ whnf [yatima| #String.concat "foo" "bar" |] `shouldBe` [yatima| "foobar" |]
    it "" $ whnf [yatima| #String.concat "foo" (#String.concat "bar" "foo") |] `shouldBe` [yatima| "foobarfoo" |]
  describe "`whnf` evaluation of BitVector operations" $ do
    it "" $ whnf [yatima| #BitVector.concat #xff #xff |] `shouldBe` [yatima| #xffff |]
