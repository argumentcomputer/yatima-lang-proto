{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yatima.Print
-- Description : Pretty-printing of expressions in the Yatima Language
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.Print where

import Control.Monad.Except
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T hiding (find)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Word
import Numeric
import Numeric.Natural
--import           Yatima.IPFS.IPLD
import Yatima.Term

-- | Pretty-printer for terms
prettyTerm :: Term -> Text
prettyTerm t = LT.toStrict $ TB.toLazyText (go t)
  where
    name :: Name -> TB.Builder
    name "" = "_"
    name x = TB.fromText x

    uses :: Uses -> TB.Builder
    uses None = "0 "
    uses Affi = "& "
    uses Once = "1 "
    uses Many = ""

    go :: Term -> TB.Builder
    go t = case t of
      Var nam _ -> name nam
      Ref nam _ _ -> name nam
      All nam use typ bod -> "∀" <> alls nam use typ bod
      Slf nam bod -> "@" <> name nam <> " " <> go bod
      New bod -> "data " <> go bod
      Use bod -> "case " <> go bod
      Lam nam bod -> "λ" <> lams nam bod
      Ann val typ -> pars (go val <> " :: " <> go typ)
      App func argm -> apps func argm
      Let rec nam use typ exp bod ->
        mconcat
          [if rec then "letrec " else "let ", uses use, name nam, ": ", go typ, " = ", go exp, "; ", go bod]
      Typ -> "Type"
      Lit lit -> TB.fromText (prettyLiteral lit)
      LTy lit -> TB.fromText (prettyLitType lit)
      Opr pri -> TB.fromText (prettyPrimOp pri)

    lams :: Name -> Term -> TB.Builder
    lams nam (Lam nam' bod') = mconcat [" ", name nam, lams nam' bod']
    lams nam bod = mconcat [" ", name nam, " => ", go bod]

    alls :: Name -> Uses -> Term -> Term -> TB.Builder
    alls nam use typ (All nam' use' typ' bod') =
      mconcat [" (", uses use, name nam, ": ", go typ, ")", alls nam' use' typ' bod']
    alls nam use typ bod =
      mconcat [" (", uses use, name nam, ": ", go typ, ")", " -> ", go bod]

    pars :: TB.Builder -> TB.Builder
    pars x = "(" <> x <> ")"

    isAtom :: Term -> Bool
    isAtom t = case t of
      Var _ _ -> True
      Ref _ _ _ -> True
      Lit _ -> True
      LTy _ -> True
      Opr _ -> True
      Ann _ _ -> True
      Typ -> True
      _ -> False

    pars' :: Term -> TB.Builder
    pars' t = if isAtom t then go t else pars (go t)

    apps :: Term -> Term -> TB.Builder
    apps f a
      | App ff fa <- f, App af aa <- a = apps ff fa <> " " <> pars (apps af aa)
      | App af aa <- a = pars' f <> " " <> pars (apps af aa)
      | App ff fa <- f = apps ff fa <> " " <> pars' a
      | otherwise = pars' f <> " " <> pars' a

prettyLiteral :: Literal -> Text
prettyLiteral t = case t of
  VWorld -> "#world"
  VNatural x -> (T.pack $ show x)
  VF64 x -> (T.pack $ show x) <> "f64"
  VF32 x -> (T.pack $ show x) <> "f32"
  VI64 x -> (T.pack $ show x) <> "u64"
  VI32 x -> (T.pack $ show x) <> "u32"
  VBitVector l x ->
    if l `mod` 4 == 0
      then "#x" <> (B16.encodeBase16 x)
      else "#b" <> (bits l . roll . B.unpack) x
  VString x -> (T.pack $ show x)
  VChar x -> T.pack $ show x
  VException -> "#exception"

roll :: [Word8] -> Integer
roll bs = foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0 bs

bits :: Natural -> Integer -> Text
bits n x
  | length digs < n' = T.pack $ replicate (n' - length digs) '0' <> digs
  | otherwise = T.pack $ digs
  where
    n' = fromIntegral n
    digs :: [Char]
    digs = showIntAtBase 2 intToDigit x ""

prettyLitType :: LitType -> Text
prettyLitType t = case t of
  TWorld -> "#World"
  TNatural -> "#Natural"
  TF64 -> "#F64"
  TF32 -> "#F32"
  TI64 -> "#I64"
  TI32 -> "#I32"
  TBitVector -> "#BitVector"
  TString -> "#String"
  TChar -> "#Char"
  TException -> "#Exception"

prettyPrimOp :: PrimOp -> Text
prettyPrimOp p = "#" <> primOpName p

prettyDef :: Name -> Def -> Text
prettyDef name (Def _ doc term typ_) =
  T.concat
    [ if doc == "" then "" else T.concat [doc, "\n"],
      name,
      ": ",
      prettyTerm $ typ_,
      "\n",
      "  = ",
      prettyTerm $ term
    ]

prettyDefs :: Map Name Def -> Text
prettyDefs defs = M.foldrWithKey go "" defs
  where
    go :: Name -> Def -> Text -> Text
    go n d txt = T.concat [prettyDef n d, "\n", txt]
