{-|
Module      : Yatima.Print
Description : Pretty-printing of expressions in the Yatima Language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE OverloadedStrings #-}
module Yatima.Print where


import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Ratio

import           Data.Text               (Text)
import qualified Data.Text               as T hiding (find)
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Builder  as TB

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.UTF8    as UTF8

import qualified Data.ByteString.Base16  as B16

import           Control.Monad.Except

import           Yatima.IPLD
import           Yatima.Term

-- | Pretty-printer for terms
prettyTerm :: Term -> Text
prettyTerm t = LT.toStrict $ TB.toLazyText (go t)
  where
    name :: Name -> TB.Builder
    name "" = "_"
    name x  = TB.fromText x

    uses :: Uses -> TB.Builder
    uses None = "0 "
    uses Affi = "& "
    uses Once = "1 "
    uses Many = ""

    go :: Term -> TB.Builder
    go t = case t of
      Hol nam                 -> "?" <> name nam
      Var nam                 -> name nam
      Ref nam                 -> name nam
      All nam use typ bod     -> "∀" <> alls nam use typ bod
      Slf nam bod             -> "@" <> name nam <> " " <> go bod
      New bod                 -> "case " <> go bod
      Use bod                 -> "data " <> go bod
      Lam nam bod             -> "λ" <> lams nam bod
      Ann val typ             -> pars (go val <> " :: " <> go typ)
      App func argm           -> apps func argm
      Let nam use typ exp bod -> mconcat
        ["let ", uses use, name nam, ": ", go typ, " = ", go exp, "; ", go bod]
      Typ                     -> "Type"
      Lit lit                 -> TB.fromText (prettyLiteral lit)
      Opr pri                 -> TB.fromText (prettyPrimOp pri)

    lams :: Name -> Term -> TB.Builder
    lams nam (Lam nam' bod') = mconcat [" ", name nam, lams nam' bod']
    lams nam bod             = mconcat [" ", name nam, " => ", go bod]

    alls :: Name -> Uses -> Term -> Term -> TB.Builder
    alls nam use typ (All nam' use' typ' bod') =
      mconcat [" (",uses use,name nam,": ",go typ,")",alls nam' use' typ' bod']
    alls nam use typ bod =
      mconcat [" (",uses use,name nam,": ",go typ,")"," -> ",go bod]

    pars :: TB.Builder -> TB.Builder
    pars x = "(" <> x <> ")"

    isAtom :: Term -> Bool
    isAtom t = case t of
      Hol _   -> True
      Var _   -> True
      Ref _   -> True
      Lit _   -> True
      Ann _ _ -> True
      _       -> False

    fun :: Term -> TB.Builder
    fun t
      | isAtom t  = go t
      | otherwise = pars (go t)

    apps :: Term -> Term -> TB.Builder
    apps f (App af aa)  = fun f <> " " <> pars (apps af aa)
    apps f a
      | isAtom f  = fun f <> " " <> pars (go a)
      | otherwise = fun f <> " " <> go a

prettyLiteral :: Literal -> Text
prettyLiteral t = case t of
  VString  x     -> (T.pack $ show $ UTF8.toString x)
  TString        -> "#String"
  TWorld         -> "#World"
  VWorld         -> "#world"
  VNatural x     -> (T.pack $ show x)
  TNatural       -> "#Natural"
  VChar    x     -> T.pack $ show x
  VBitString x   -> "0x" <> (B16.encodeBase16 x)
  VBitVector l x -> (T.pack $ show l) <> "x" <> (B16.encodeBase16 x)
  TBitVector l   -> "#BitVector" <> (T.pack $ show l)
  TChar          -> "#Char"
  TBitString     -> "#BitString"

prettyPrimOp :: PrimOp -> Text
prettyPrimOp p = "#" <> primOpName p

prettyDef :: Name -> Def -> Text
prettyDef name (Def doc term typ_) = T.concat
  [ if doc == "" then "" else T.concat [doc,"\n"]
  , name,": ", prettyTerm $ typ_, "\n"
  , "  = ", prettyTerm $ term
  ]

prettyDefs :: Map Name Def -> Text
prettyDefs defs = M.foldrWithKey go "" defs
  where
    go :: Name -> Def -> Text -> Text
    go n d txt = T.concat [prettyDef n d, "\n" , txt]
