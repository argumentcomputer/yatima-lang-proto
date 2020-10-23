module Yatima.IR where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Yatima.Uses
import           Yatima.Print
import           Yatima.Term

data IR where
  RefI :: Name -> IR
  VarI :: Name -> IR
  LamI :: Uses -> Name -> IR -> IR
  AppI :: Uses -> IR -> IR -> IR
  NewI :: IR -> IR
  UseI :: IR -> Maybe LitType -> IR
  LetI :: Uses -> Name -> IR -> IR -> IR
  -- Typing constructors
  AllI :: Name -> Uses -> IR -> IR -> IR
  SlfI :: Name -> IR -> IR
  TypI :: IR
  -- Native datatypes
  LitI :: Literal -> IR
  LTyI :: LitType -> IR
  OprI :: PrimOp  -> IR

-- Transform an IR into Scheme code. The `Int` variable `dep` represents the depth of the indentation
-- All defined variables start with ':' so to not collide with any primitive Scheme value or keyword
defToScheme :: Text -> IR -> Text
defToScheme nam trm = T.concat ["(define :", nam, "\n", termToScheme 2 True Once trm, ")\n\n"]

termToScheme :: Int -> Bool -> Uses -> IR -> Text
termToScheme dep True  use trm = T.concat [T.concat [" " | _ <- [1..dep]], termToScheme dep False use trm]
termToScheme dep False use trm = case trm of
  RefI nam -> T.concat [":", nam]
  VarI nam -> T.concat [":", nam]
  LamI argUse nam bod ->
    case argUse of
      None -> termToScheme dep False use bod
      _    -> T.concat ["(lambda (:", nam, ")\n", termToScheme (dep+2) True use bod, ")"]
  AppI argUse fun arg ->
    case argUse of
      None -> termToScheme dep False use fun
      _    -> T.concat ["(", termToScheme (dep+1) False use fun, "\n", termToScheme (dep+1) True (use *# argUse) arg, ")"]
  LetI valUse nam val bod ->
    case valUse of
      None -> termToScheme dep False use bod
      _    -> T.concat ["(letrec ((:", nam, " ", termToScheme (dep + 12 + T.length nam) False (use *# valUse) val, "))\n", termToScheme (dep+2) True use bod, ")"]
  NewI exp            -> termToScheme dep False use exp
  UseI exp Nothing    -> termToScheme dep False use exp
  UseI exp (Just typ) -> termToScheme dep False use exp -- TODO
  -- Native datatypes
  LitI lit -> litToScheme lit
  OprI opr -> oprToScheme opr
  -- Types are erased
  AllI nam use typ bod -> "'()"
  SlfI nam bod         -> "'()"
  TypI                 -> "'()"
  LTyI typ             -> "'()"

litToScheme :: Literal -> Text
litToScheme lit = case lit of
  VNatural x -> T.pack $ show x
  _          -> "'()" -- TODO

oprToScheme :: PrimOp  -> Text
oprToScheme opr = case opr of
  Natural_succ -> "+1"
  _            -> "'()" -- TODO
