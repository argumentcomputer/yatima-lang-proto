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

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Control.Monad.Except

import Language.Yatima.Term
import Language.Yatima.Defs

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
      Var n          -> n
      Ref n          -> n
      Lam n ut b     -> T.concat ["λ", lams n ut b]
      App f a        -> apps f a
      All "" n u t b -> T.concat ["∀", alls n u t b]
      All s n u t b  -> T.concat ["@", s, " ∀", alls n u t b]
      Typ            -> "Type"
      Let n u t x b  -> T.concat ["let ",uses u,name n,": ",go t," = ",go x,";\n",go b]

    lams :: Name -> Maybe (Uses, Term) -> Term -> Text
    lams n ut b = case b of
       Lam n' ut' b' -> T.concat [txt, lams n' ut' b']
       _             -> T.concat [txt, " => ", go b]
       where
         txt = case ut of
            Nothing    -> T.concat [" ", n]
            Just (u,t) -> T.concat [" (", uses u, n,": ", go t,")"]

    alls :: Name -> Uses -> Term -> Term -> Text
    alls n u t b = case b of
      All _ n' u' t' b' -> T.concat [txt, alls n' u' t' b']
      _                 -> T.concat [txt, " -> ", go b]
      where
        txt = case (n, u, t) of
          ("", Many, t) -> T.concat [" ", go t]
          _             -> T.concat [" (", uses u, n,": ", go t,")"]

    apps :: Term -> Term -> Text
    apps f@(Lam _ _ _) a      = T.concat ["(", go f, ") ", go a]
    apps f@(All _ _ _ _ _) a  = T.concat ["(", go f, ") ", go a]
    apps f@(Let _ _ _ _ _) a  = T.concat ["(", go f, ") ", go a]
    apps f  a@(Lam _ _ _)     = T.concat [go f, " (", go a, ")"]
    apps f (App af aa)        = T.concat [go f, " ", "(", apps af aa,")"]
    apps (App af aa) a        = T.concat [apps af aa, " ", go a]
    apps f a                  = T.concat [go f, " ", go a]


prettyDef :: Def -> Text
prettyDef (Def name doc term typ_) = T.concat 
  [ if doc == "" then "" else T.concat [doc,"\n"]
  , name,": ", prettyTerm $ typ_, "\n"
  , "  = ", prettyTerm $ term
  ]

prettyDefs :: Index -> Cache -> Either DerefErr Text
prettyDefs index cache = M.foldrWithKey go (Right "") index
  where
    go :: Name -> CID -> Either DerefErr Text -> Either DerefErr Text
    go n c (Left e)    = Left e
    go n c (Right txt) = case runExcept (derefMetaDefCID n c index cache) of
      Left e   -> Left e
      Right d  -> return $ T.concat
        [ txt,"\n"
        , n,"#", T.pack $ show c,"\n"
        , prettyDef d, "\n"
        ]
