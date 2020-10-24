module Yatima.Compiler.Scheme where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Yatima.Uses
import           Yatima.PrimOp
import           Yatima.Literal
import           Yatima.IR

type Name = Text

data Scheme where
  Define   :: Name     -> Scheme -> Scheme
  Variable :: Name     -> Scheme
  Lambda   :: Name     -> Scheme -> Scheme
  Apply    :: Scheme   -> Scheme -> Scheme
  Let      :: Name     -> Scheme -> Scheme -> Scheme
  Letrec   :: Name     -> Scheme -> Scheme -> Scheme
  Value    :: Literal  -> Scheme
  Operator :: PrimOp   -> Scheme
  Begin    :: [Scheme] -> Scheme -> Scheme
  Assign   :: Name     -> Scheme -> Scheme
  Equal    :: Scheme   -> Scheme -> Scheme
  If       :: Scheme   -> Scheme -> Scheme -> Scheme
  Nil      :: Scheme

-- Transform an IR into Scheme AST.
--- All defined variables will start with ':' so to not collide with any primitive Scheme value or keyword
defToScheme :: Text -> IR -> Scheme
defToScheme nam trm = Define (T.cons ':' nam) (termToScheme Once trm)

termToScheme :: Uses -> IR -> Scheme
termToScheme use trm = case trm of
  RefI nam -> Variable (nonPrim nam)
  VarI nam -> Variable (nonPrim nam)
  LamI argUse nam bod ->
    case argUse of
      None -> termToScheme use bod
      _    -> Lambda (nonPrim nam) (termToScheme use bod)
  AppI argUse fun arg ->
    case argUse of
      None -> termToScheme use fun
      _    -> Apply (termToScheme use fun) (termToScheme (use *# argUse) arg)
  LetI rec valUse nam val bod ->
    case valUse of
      None -> termToScheme use bod
      _    -> (if rec then Letrec else Let) (nonPrim nam) (termToScheme (use *# valUse) val) (termToScheme use bod)
  NewI exp            -> termToScheme use exp
  UseI exp Nothing    -> termToScheme use exp
  UseI exp (Just TNatural) ->
    Lambda "zero_case" $
    Lambda "succ_case" $
    Let "tmp" (termToScheme use exp) $
    If (Equal (Variable "tmp") (Value (VNatural 0))) (Variable "zero_case") $
    Apply (Variable "succ_case") (Apply (Operator Natural_pred) (Variable "tmp"))
  UseI exp (Just typ) -> termToScheme use exp -- TODO
  -- Native datatypes
  LitI lit -> Value lit
  OprI opr -> Operator opr
  -- Types are erased
  AllI nam use typ bod -> Nil
  SlfI nam bod         -> Nil
  TypI                 -> Nil
  LTyI typ             -> Nil
  where nonPrim nam = T.cons ':' nam

--- Transform Scheme AST into code. The `Int` variable `dep` represents the depth of the indentation
schemeToCode :: Int -> Bool -> Scheme -> Text
schemeToCode dep True  trm = T.concat [T.replicate dep " ", schemeToCode dep False trm]
schemeToCode dep False trm = case trm of
  Define   nam  bod      -> T.concat ["(define ", nam, "\n", schemeToCode 2 True bod, ")\n\n"]
  Variable nam           -> nam
  Lambda   nam  bod      -> T.concat ["(lambda (", nam, ")\n", schemeToCode (dep+2) True bod, ")"]
  Apply    fun  arg      -> T.concat ["(", schemeToCode (dep+1) False fun, "\n", schemeToCode (dep+1) True arg, ")"]
  Let      nam  exp  bod -> T.concat ["(let ((", nam, " ",
                                      schemeToCode (dep+8+T.length nam) False exp, "))\n",
                                      schemeToCode (dep+2) True bod, ")"]
  Letrec   nam  exp  bod -> T.concat ["(letrec ((", nam, " ",
                                      schemeToCode (dep+11+T.length nam) False exp, "))\n",
                                      schemeToCode (dep+2) True bod, ")"]
  Value    lit           -> litToCode lit
  Operator opr           -> oprToCode opr
  Begin    exps end      -> T.concat ["(begin\n"
                                     , T.concat $ fmap (\exp -> T.append (schemeToCode (dep+2) True exp) "\n") exps,
                                       schemeToCode (dep+2) True end, ")"]
  Assign   nam  exp      -> T.concat ["(set! ", nam, schemeToCode (dep+7+T.length nam) False exp, ")"]
  Equal    exp1 exp2     -> T.concat ["(= ", schemeToCode (dep+3) False exp1, "\n", schemeToCode (dep+3) True exp2, ")"]
  If       exp  tru  fal -> T.concat ["(if ", schemeToCode (dep+4) False exp, "\n", schemeToCode (dep+4) True tru, "\n", schemeToCode (dep+4) True fal, ")"]
  Nil                    -> "'()"

defToCode :: Text -> IR -> Text
defToCode nam trm = schemeToCode 0 False $ defToScheme nam trm

litToCode :: Literal -> Text
litToCode lit = case lit of
  VNatural x -> T.pack $ show x
  _          -> "'()" -- TODO

oprToCode :: PrimOp  -> Text
oprToCode opr = case opr of
  Natural_succ -> "1+"
  Natural_pred -> "1-"
  Natural_add  -> "(lambda (x) (lambda (y) (+ x y)))"
  Natural_mul  -> "(lambda (x) (lambda (y) (* x y)))"
  Natural_sub  -> "(lambda (x) (lambda (y) (- x y)))"
  Natural_div  -> "(lambda (x) (lambda (y) (div x y)))"
  Natural_mod  -> "(lambda (x) (lambda (y) (mod x y)))"
  Natural_gt   -> "(lambda (x) (lambda (y) (> x y)))"
  Natural_ge   -> "(lambda (x) (lambda (y) (>= x y)))"
  Natural_eq   -> "(lambda (x) (lambda (y) (= x y)))"
  Natural_ne   -> "(lambda (x) (lambda (y) (not (= x y))))"
  Natural_lt   -> "(lambda (x) (lambda (y) (< x y)))"
  Natural_le   -> "(lambda (x) (lambda (y) (<= x y)))"
  _            -> "'()" -- TODO
