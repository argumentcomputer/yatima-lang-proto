{-|
Module      : Language.Yatima.Print
Description : Pretty-printing of expressions in the Yatima Language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Yatima.Print
  ( prettyTerm
  , prettyDef
  , prettyDefs
  , prettyConstant
  ) where

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

import           Language.Yatima.IPLD
import           Language.Yatima.Term

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
      Lit lit                 -> TB.fromText (prettyConstant lit)

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

prettyConstant :: Constant -> Text
prettyConstant t = case t of
  CStr x   -> (T.pack $ show $ UTF8.toString x)
  CInt x   -> (T.pack $ show x)
  CNat x   -> (T.pack $ show x) <> "n"
  CChr x   -> T.pack $ show x
  CBit x   -> "'x" <> (B16.encodeBase16 x)
  CRat x   -> (T.pack $ show $ numerator x) <> "/" <> (T.pack $ show $ denominator x)
  CWrd l x -> (T.pack $ show x) <> "u" <> (T.pack $ show l)
  TWrd l   -> "#Word" <> (T.pack $ show l)
  TStr   -> "#String"
  TChr   -> "#Char"
  TInt   -> "#Integer"
  TNat   -> "#Natural"
  TRat   -> "#Rational"
  TBit   -> "#Bits"
  TUni   -> "#World"
  CUni   -> "#world"

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
