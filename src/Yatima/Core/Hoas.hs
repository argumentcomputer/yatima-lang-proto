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
  VarH :: Loc -> Name -> Int -> Hoas
  RefH :: Loc -> Name -> Cid -> Cid -> Hoas
  LamH :: Loc -> Name -> (Hoas -> Hoas) -> Hoas
  AppH :: Loc -> Hoas -> Hoas -> Hoas
  NewH :: Loc -> Hoas -> Hoas
  UseH :: Loc -> Hoas -> Hoas
  LetH :: Loc -> Name -> Uses -> Hoas -> Hoas -> (Hoas -> Hoas) -> Hoas
  AllH :: Loc -> Name -> Uses -> Hoas -> (Hoas -> Hoas) -> Hoas
  SlfH :: Loc -> Name -> (Hoas -> Hoas) -> Hoas
  FixH :: Loc -> Name -> (Hoas -> Hoas) -> Hoas
  AnnH :: Loc -> Hoas -> Hoas -> Hoas
  UnrH :: Loc -> Name -> Int -> Hoas -> Hoas -> Hoas
  TypH :: Loc -> Hoas
  LitH :: Loc -> Literal -> Hoas
  LTyH :: Loc -> LitType -> Hoas
  OprH :: Loc -> PrimOp -> Hoas

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
  Typ loc -> TypH loc
  Var loc nam idx ->
    if idx < length clos
      then clos !! idx
      else VarH loc nam (length clos - idx)
  Ref loc nam def trm -> RefH loc nam def trm
  Lam loc nam bod -> LamH loc nam (bind bod)
  App loc fun arg -> AppH loc (go fun) (go arg)
  New loc exp -> NewH loc (go exp)
  Use loc exp -> UseH loc (go exp)
  Ann loc val typ -> AnnH loc (go val) (go typ)
  Let loc rec nam use typ exp bod ->
    LetH loc nam use (go typ) (fix loc rec nam exp) (bind bod)
  All loc nam use typ bod -> AllH loc nam use (go typ) (bind bod)
  Slf loc nam bod -> SlfH loc nam (bind bod)
  Lit loc lit -> LitH loc lit
  LTy loc lty -> LTyH loc lty
  Opr loc opr -> OprH loc opr
  where
    go t = termToHoas clos t
    bind t = (\x -> termToHoas (x : clos) t)
    fix l r n t = if r then FixH l n (bind t) else go t

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: Int -> Hoas -> Term
hoasToTerm dep t = case t of
  TypH loc -> Typ loc
  RefH loc nam def trm -> Ref loc nam def trm
  VarH loc nam idx -> Var loc nam $ if idx < 0 then idx + dep else idx
  LamH loc nam bod -> Lam loc nam (bind nam bod)
  AppH loc fun arg -> App loc (go fun) (go arg)
  UseH loc exp -> Use loc (go exp)
  NewH loc exp -> New loc (go exp)
  LetH loc nam use typ exp bod ->
    Let loc (isFix exp) nam use (go typ) (go exp) (bind nam bod)
  AllH loc nam use typ bod -> All loc nam use (go typ) (bind nam bod)
  SlfH loc nam bod -> Slf loc nam (bind nam bod)
  FixH _ nam bod -> bind nam bod
  AnnH loc trm typ -> Ann loc (go trm) (go typ)
  UnrH _ _ _ trm _ -> go trm
  LitH loc lit -> Lit loc lit
  LTyH loc lty -> LTy loc lty
  OprH loc opr -> Opr loc opr
  where
    go t = hoasToTerm dep t
    bind n b = hoasToTerm (dep + 1) (b (VarH NoLoc n (- dep -1)))
    isFix (FixH _ _ _) = True
    isFix _ = False

printHoas :: Hoas -> Text
printHoas = prettyTerm . (hoasToTerm 0)

instance Show Hoas where
  show t = T.unpack $ printHoas t

defToHoas :: Name -> Def -> (Hoas, Hoas)
defToHoas name (Def loc _ _ term typ_) =
  (FixH loc name (\s -> termToHoas [s] term), termToHoas [] typ_)

hoasLoc :: Hoas -> Loc
hoasLoc t = case t of
  TypH loc -> loc
  RefH loc _ _ _ -> loc
  VarH loc _ _ -> loc
  LamH loc _ _ -> loc
  AppH loc _ _ -> loc
  UseH loc _ -> loc
  NewH loc _ -> loc
  LetH loc _ _ _ _ _ -> loc
  AllH loc _ _ _ _ -> loc
  SlfH loc _ _ -> loc
  FixH loc _ _ -> loc
  AnnH loc _ _ -> loc
  UnrH loc _ _ _ _ -> loc
  LitH loc _ -> loc
  LTyH loc _ -> loc
  OprH loc _ -> loc
