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
  ) where

import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
 
import qualified Data.Text               as T hiding (find)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Builder  as TB

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
      All ""  nam use typ bod -> "∀" <> alls nam use typ bod
      All slf nam use typ bod -> "@" <> name slf <> " ∀" <> alls nam use typ bod
      Lam nam bod             -> "λ" <> lams nam bod
      Ann val typ             -> pars (go val <> " :: " <> go typ)
      App func argm           -> apps func argm
      Let nam use typ exp bod -> mconcat
        ["let ", uses use, name nam, ": ", go typ, " = ", go exp, "; ", go bod]
      Typ                     -> "*"

    lams :: Name -> Term -> TB.Builder
    lams nam (Lam nam' bod') = mconcat [" ", name nam, lams nam' bod']
    lams nam bod             = mconcat [" ", name nam, " => ", go bod]

    alls :: Name -> Uses -> Term -> Term -> TB.Builder
    alls nam use typ (All "" nam' use' typ' bod') = 
      mconcat [" (",uses use,name nam,": ",go typ,")",alls nam' use' typ' bod']
    alls nam use typ bod = 
      mconcat [" (",uses use,name nam,": ",go typ,")"," -> ",go bod]

    pars :: TB.Builder -> TB.Builder
    pars x = "(" <> x <> ")"

    fun :: Term -> TB.Builder
    fun t = case t of
      Lam _ _       -> pars (go t)
      All _ _ _ _ _ -> pars (go t)
      Let _ _ _ _ _ -> pars (go t)
      _             -> go t

    apps :: Term -> Term -> TB.Builder
    apps f a = case a of
      (App af aa)       -> fun f <> " " <> pars (apps af aa)
      (Lam _ _)         -> fun f <> " " <> pars (go a)
      (All _ _ _ _ _)   -> fun f <> " " <> pars (go a)
      (Let _ _ _ _ _)   -> fun f <> " " <> pars (go a)
      _                 -> fun f <> " " <> go a

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
