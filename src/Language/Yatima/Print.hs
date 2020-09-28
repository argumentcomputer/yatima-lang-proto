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
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)

import Language.Yatima.Term

-- | Pretty-printer for terms
prettyTerm :: Term -> Text
prettyTerm t = go t
  where
    name "" = "_"
    name x  = x

    go :: Term -> Text
    go t = case t of
      Hol name                -> T.concat ["?", name]
      Var name                -> name
      All name uses bind body -> T.concat ["∀ ", alls name bind body]
      Lam name body           -> T.concat ["λ ", lams name body]
      App func argm           -> apps func argm
      Any                     -> "*"

    alls :: Name -> Term -> Term -> Text
    alls name bind body = case body of
      All name' uses' bind' body' -> T.concat ["(", name, ": ", go bind, ") ", alls name' bind' body']
      _                           -> T.concat ["(", name, ": ", go bind, ") => ", go body]

    lams :: Name -> Term -> Text
    lams n b = case b of
      Lam n' b' -> T.concat [n, " ", lams n' b']
      _         -> T.concat [n, " => ", go b]

    apps :: Term -> Term -> Text
    apps f@(Lam _ _) a  = T.concat ["(", go f, ") ", go a]
    apps f  a@(Lam _ _) = T.concat [go f, " (", go a, ")"]
    apps f (App af aa)  = T.concat [go f, " ", "(", apps af aa,")"]
    apps (App af aa) a  = T.concat [apps af aa, " ", go a]
    apps f a            = T.concat [go f, " ", go a]
