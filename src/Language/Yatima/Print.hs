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
      Var n i -> n
      Lam n b -> T.concat ["λ", lams n b]
      App f a -> apps f a

    lams :: Name -> Term -> Text
    lams n b = 
      let txt = T.concat [" (", n, ")"]
      in case b of
        Lam n' b' -> T.concat [txt, lams n' b']
        _         -> T.concat [txt, " => ", go b]

    apps :: Term -> Term -> Text
    apps f@(Lam _ _) a  = T.concat ["(", go f, ") ", go a]
    apps f  a@(Lam _ _) = T.concat [go f, " (", go a, ")"]
    apps f (App af aa)  = T.concat [go f, " ", "(", apps af aa,")"]
    apps (App af aa) a  = T.concat [apps af aa, " ", go a]
    apps f a            = T.concat [go f, " ", go a]
