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
  UseI :: IR -> IR
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
termToScheme :: Int -> Bool -> Uses -> IR -> Text
termToScheme dep True  use trm = T.concat [T.concat [" " | _ <- [1..dep]], termToScheme dep False use trm]
termToScheme dep False use trm = case trm of
  RefI nam -> nam
  VarI nam -> nam
  LamI argUse nam bod ->
    case argUse of
      None -> termToScheme dep False use bod
      _    -> T.concat ["(lambda (", nam, ")\n", termToScheme (dep+2) True use bod, ")"]
  AppI argUse fun arg ->
    case argUse of
      None -> termToScheme dep False use fun
      _    -> T.concat ["(", termToScheme (dep+1) False use fun, "\n", termToScheme (dep+1) True (use *# argUse) arg, ")"]
  LetI valUse nam val bod -> 
    case valUse of
      None -> termToScheme dep False use bod
      _    -> T.concat ["(letrec ((", nam, " ", termToScheme (dep + 11 + T.length nam) False (use *# valUse) val, "))\n", termToScheme (dep+2) True use bod, ")"]
  NewI exp -> termToScheme dep False use exp
  UseI exp -> termToScheme dep False use exp
  -- Native datatypes (TODO)
  LitI lit -> "'()"
  OprI opr -> "'()"
  -- Types are erased
  AllI nam use typ bod -> "'()"
  SlfI nam bod         -> "'()"
  TypI -> "'()"
  LTyI typ -> "'()"

defToScheme :: Text -> IR -> Text
defToScheme nam trm = T.concat ["(define  ", nam, "\n", termToScheme 2 True Once trm, ")\n\n"]
