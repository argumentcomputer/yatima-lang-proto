{-|
Module      : Language.Yatima.Print
Description : Pretty-printing of expressions in the Yatima Language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
module Language.Yatima.Print
  ( prettyTerm
  , prettyDef
  , prettyDefs
  ) where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T hiding (find)

import           Control.Monad.Except

import           Language.Yatima.IPLD
import           Language.Yatima.Term

-- | Pretty-printer for terms
prettyTerm :: Term -> Text
prettyTerm t = go t
  where
    name "" = "_"
    name x  = x

    uses :: Uses -> Text
    uses None = "0 "
    uses Affi = "& "
    uses Once = "1 "
    uses Many = ""

    go :: Term -> Text
    go t = case t of
      Hol name                     -> T.concat ["?", name]
      Var name                     -> name
      Ref name                     -> name
      All ""   name used bind body -> T.concat ["∀", alls name used bind body]
      All self name used bind body ->
        T.concat ["@", self, " ∀", alls name used bind body]
      Lam name body           -> T.concat ["λ", lams name body]
      App func argm           -> apps func argm
      Let name used typ_ expr body  -> T.concat
        ["let ", uses used, name, ": ", go typ_, " = ", go expr, ";\n", go body]
      Any                     -> "*"

    lams :: Name -> Term -> Text
    lams name (Lam name' body') = T.concat [" ", name, lams name' body']
    lams name body              = T.concat [" ", name, " => ", go body]

    alls :: Name -> Uses -> Term -> Term -> Text
    alls name used bind (All "" name' used' bind' body') = T.concat 
      [allHead name used bind, alls name' used' bind' body']
    alls name used bind body = T.concat 
      [allHead name used bind, " -> ", go body]

    allHead ""   Many body = T.concat [" ", go t]
    allHead name used body = T.concat [" (", uses used, name,": ", go body,")"]

    apps :: Term -> Term -> Text
    apps f a = case (f,a) of
      (Lam _ _, a)       -> T.concat ["(", go f, ") ", go a]
      (All _ _ _ _ _, a) -> T.concat ["(", go f, ") ", go a]
      (Let _ _ _ _ _, a) -> T.concat ["(", go f, ") ", go a]
      (f,Lam _ _)        -> T.concat [go f, " (", go a, ")"]
      (f,App af aa)      -> T.concat [go f, " ", "(", apps af aa,")"]
      (App af aa, a)     -> T.concat [apps af aa, " ", go a]
      _                  -> T.concat [go f, " ", go a]


prettyDef :: Def -> Text
prettyDef (Def name doc term typ_) = T.concat 
  [ if doc == "" then "" else T.concat [doc,"\n"]
  , name,": ", prettyTerm $ typ_, "\n"
  , "  = ", prettyTerm $ term
  ]

prettyDefs :: Index -> Cache -> Either IPLDErr Text
prettyDefs index cache = M.foldrWithKey go (Right "") index
  where
    go :: Name -> CID -> Either IPLDErr Text -> Either IPLDErr Text
    go n c (Left e)    = Left e
    go n c (Right txt) = case runExcept (derefMetaDefCID n c index cache) of
      Left e   -> Left e
      Right d  -> return $ T.concat
        [ txt,"\n"
        , printCIDBase32 c,"\n"
        , prettyDef d, "\n"
        ]
