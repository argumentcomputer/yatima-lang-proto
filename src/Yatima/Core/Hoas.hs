module Yatima.Core.Hoas where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Yatima.Core.Ctx            (Ctx (..), (<|))
import qualified Yatima.Core.Ctx            as Ctx

import           Yatima.Print
import           Yatima.Term

-- | Higher-Order Abstract Syntax
data Hoas where
  VarH :: Name -> Int -> Hoas
  RefH :: Name -> Hoas
  LamH :: Name -> (Hoas -> Hoas) -> Hoas
  AppH :: Hoas -> Hoas -> Hoas
  NewH :: Hoas -> Hoas
  UseH :: Hoas -> Hoas
  LetH :: Name -> Uses -> Hoas -> Hoas -> (Hoas -> Hoas) -> Hoas
  AllH :: Name -> Uses -> Hoas -> (Hoas -> Hoas) -> Hoas
  SlfH :: Name -> (Hoas -> Hoas) -> Hoas
  FixH :: Name -> (Hoas -> Hoas) -> Hoas
  AnnH :: Hoas -> Hoas -> Hoas
  UnrH :: Int  -> Hoas -> Hoas -> Hoas
  TypH :: Hoas
  HolH :: Name -> Hoas
  LitH :: Literal -> Hoas
  LTyH :: LitType -> Hoas
  OprH :: PrimOp  -> Hoas
  WhnH :: Hoas    -> Hoas

type PreContext = Ctx Hoas
type Context    = Ctx (Uses,Hoas)

mulCtx :: Uses -> Context -> Context
mulCtx Once ctx = ctx
mulCtx uses ctx = fmap (\(uses', typ) -> (uses *# uses', typ)) ctx

-- Assumes both context are compatible
addCtx :: Context -> Context -> Context
addCtx = Ctx.zipWith (\(uses,typ) (uses',_) -> (uses +# uses', typ))

toContext :: PreContext -> Context
toContext = fmap (\(term) -> (None, term))

-- | A filled hole
type Hole = (Name, Hoas)

-- | Convert a lower-order `Term` to a GHC higher-order one
termToHoas :: PreContext -> Term -> Hoas
termToHoas ctx t = case t of
  Typ                     -> TypH
  Hol nam                 -> HolH nam
  Var nam                 -> maybe (VarH nam 0) id (Ctx.find nam ctx)
  Ref nam                 -> RefH nam
  Lam nam bod             -> LamH nam (bind nam bod)
  App fun arg             -> AppH (go fun) (go arg)
  New exp                 -> NewH (go exp)
  Use exp                 -> UseH (go exp)
  Ann val typ             -> AnnH (go val) (go typ)
  Let nam use typ exp bod -> LetH nam use (go typ) (rec nam exp) (bind nam bod)
  All nam use typ bod     -> AllH nam use (go typ) (bind nam bod)
  Slf nam bod             -> SlfH nam (bind nam bod)
  Lit lit                 -> LitH lit
  LTy lty                 -> LTyH lty
  Opr opr                 -> OprH opr
  where
    go      t   = termToHoas ctx t
    bind  n t   = (\x   -> termToHoas ((n,x)<|ctx) t)
    rec n t = FixH n (bind n t)

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: PreContext -> Hoas -> Term
hoasToTerm ctx t = case t of
  TypH                     -> Typ
  HolH nam                 -> Hol nam
  RefH nam                 -> Ref nam
  VarH nam idx             -> Var nam
  LamH nam bod             -> Lam nam (bind nam bod)
  AppH fun arg             -> App (go fun) (go arg)
  UseH exp                 -> Use (go exp)
  NewH exp                 -> New (go exp)
  LetH nam use typ exp bod -> Let nam use (go typ) (go exp) (bind nam bod)
  AllH nam use typ bod     -> All nam use (go typ) (bind nam bod)
  SlfH nam bod             -> Slf nam (bind nam bod)
  FixH nam bod             -> bind nam bod
  AnnH trm typ             -> Ann (go trm) (go typ)
  UnrH _   trm _           -> go trm
  LitH lit                 -> Lit lit
  LTyH lty                 -> LTy lty
  OprH opr                 -> Opr opr
  where
    dep          = Ctx.depth ctx
    go t         = hoasToTerm ctx t
    bind n b     = hoasToTerm ((n,TypH)<|ctx) (b (VarH n dep))

printHoas :: Hoas -> Text
printHoas = prettyTerm . (hoasToTerm Ctx.empty)

instance Show Hoas where
 show t = T.unpack $ printHoas t

defToHoas :: Name -> Def -> (Hoas,Hoas)
defToHoas name (Def _ term typ_) =
  ( FixH name (\s -> termToHoas (Ctx.singleton (name,s)) term)
  , FixH name (\s -> termToHoas (Ctx.singleton (name,s)) typ_)
  )
