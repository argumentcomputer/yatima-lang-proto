-- |
-- Module      : Yatima.Core.Hoas
-- Description : Defines a Higher-Order-Abstract-Syntax for Yatima terms
-- typechecking.
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.Core.Hoas where

import Data.IPLD.Cid
import Data.Text (Text)
import qualified Data.Text as T
import Yatima.Core.Ctx (Ctx (..))
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Print
import Yatima.Term

-- | Higher-Order Abstract Syntax
data Hoas where
  VarH :: Name -> Int -> Hoas
  RefH :: Name -> Cid -> Cid -> Hoas
  LamH :: Name -> (Hoas -> Hoas) -> Hoas
  AppH :: Hoas -> Hoas -> Hoas
  NewH :: Hoas -> Hoas
  UseH :: Hoas -> Hoas
  LetH :: Name -> Uses -> Hoas -> Hoas -> (Hoas -> Hoas) -> Hoas
  AllH :: Name -> Uses -> Hoas -> (Hoas -> Hoas) -> Hoas
  SlfH :: Name -> (Hoas -> Hoas) -> Hoas
  FixH :: Name -> (Hoas -> Hoas) -> Hoas
  AnnH :: Hoas -> Hoas -> Hoas
  UnrH :: Name -> Int -> Hoas -> Hoas -> Hoas
  TypH :: Hoas
  LitH :: Literal -> Hoas
  LTyH :: LitType -> Hoas
  OprH :: PrimOp -> Hoas

type PreContext = Ctx Hoas

type Context = Ctx (Uses, Hoas)

mulCtx :: Uses -> Context -> Context
mulCtx Once ctx = ctx
mulCtx uses ctx = fmap (\(uses', typ) -> (uses *# uses', typ)) ctx

-- Assumes both contexts are compatible
addCtx :: Context -> Context -> Context
addCtx = Ctx.zipWith (\(uses, typ) (uses', _) -> (uses +# uses', typ))

toContext :: PreContext -> Context
toContext = fmap (\(term) -> (None, term))

-- | Convert a lower-order `Term` to a GHC higher-order one
termToHoas :: [Hoas] -> Term -> Hoas
termToHoas clos t = case t of
  Typ -> TypH
  Var nam idx ->
    if idx < length clos
      then clos !! idx
      else VarH nam (length clos - idx)
  Ref nam def trm -> RefH nam def trm
  Lam nam bod -> LamH nam (bind nam bod)
  App fun arg -> AppH (go fun) (go arg)
  New exp -> NewH (go exp)
  Use exp -> UseH (go exp)
  Ann val typ -> AnnH (go val) (go typ)
  Let rec nam use typ exp bod ->
    LetH nam use (go typ) (fix rec nam exp) (bind nam bod)
  All nam use typ bod -> AllH nam use (go typ) (bind nam bod)
  Slf nam bod -> SlfH nam (bind nam bod)
  Lit lit -> LitH lit
  LTy lty -> LTyH lty
  Opr opr -> OprH opr
  where
    go t = termToHoas clos t
    bind n t = (\x -> termToHoas (x : clos) t)
    fix r n t = if r then FixH n (bind n t) else go t

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: Int -> Hoas -> Term
hoasToTerm dep t = case t of
  TypH -> Typ
  RefH nam def trm -> Ref nam def trm
  VarH nam idx -> Var nam $ if idx < 0 then idx + dep else idx
  LamH nam bod -> Lam nam (bind nam bod)
  AppH fun arg -> App (go fun) (go arg)
  UseH exp -> Use (go exp)
  NewH exp -> New (go exp)
  LetH nam use typ exp bod ->
    Let (isFix exp) nam use (go typ) (go exp) (bind nam bod)
  AllH nam use typ bod -> All nam use (go typ) (bind nam bod)
  SlfH nam bod -> Slf nam (bind nam bod)
  FixH nam bod -> bind nam bod
  AnnH trm typ -> Ann (go trm) (go typ)
  UnrH _ _ trm _ -> go trm
  LitH lit -> Lit lit
  LTyH lty -> LTy lty
  OprH opr -> Opr opr
  where
    go t = hoasToTerm dep t
    bind n b = hoasToTerm (dep + 1) (b (VarH n (- dep -1)))
    isFix exp@(FixH _ _) = True
    isFix exp = False

printHoas :: Hoas -> Text
printHoas = prettyTerm . (hoasToTerm 0)

instance Show Hoas where
  show t = T.unpack $ printHoas t

defToHoas :: Name -> Def -> (Hoas, Hoas)
defToHoas name (Def _ _ term typ_) =
  (FixH name (\s -> termToHoas [s] term), termToHoas [] typ_)
