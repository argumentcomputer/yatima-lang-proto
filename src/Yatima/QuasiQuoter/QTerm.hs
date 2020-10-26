module Yatima.QuasiQuoter.QTerm where

import           Yatima.Term

data QTerm where
  QHol :: Name -> QTerm
  QVar :: Name -> QTerm
  QAll :: Name -> Uses -> QTerm -> QTerm -> QTerm
  QLam :: Name -> QTerm -> QTerm
  QApp :: QTerm -> QTerm -> QTerm
  QSlf :: Name -> QTerm -> QTerm
  QNew :: QTerm -> QTerm
  QUse :: QTerm -> QTerm
  QRef :: Name -> QTerm
  QLet :: Bool -> Name -> Uses -> QTerm -> QTerm -> QTerm -> QTerm
  QTyp :: QTerm
  QAnn :: QTerm -> QTerm -> QTerm
  QLit :: Literal -> QTerm
  QLTy :: LitType -> QTerm
  QOpr :: PrimOp -> QTerm
  QAnt :: Name -> QTerm

toQTerm :: Term -> QTerm
toQTerm t = case t of
  Hol x           -> QHol x
  Var x           -> QVar x
  All n u t b     -> QAll n u (go t) (go b)
  Lam n b         -> QLam n (go b)
  App f a         -> QApp (go f) (go a)
  Slf n t         -> QSlf n (go t)
  New t           -> QNew (go t)
  Use t           -> QUse (go t)
  Ref n           -> QRef n
  Let r n u t x b -> QLet r n u (go t) (go x) (go b)
  Typ             -> QTyp
  Ann x t         -> QAnn (go x) (go t)
  Lit x           -> QLit x
  LTy x           -> QLTy x
  Opr x           -> QOpr x
  where
    go = toQTerm
