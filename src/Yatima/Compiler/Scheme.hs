{-# LANGUAGE OverloadedStrings #-}
module Yatima.Compiler.Scheme where

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TB

import           Yatima.Core.IR
import           Yatima.Core.Prim
import           Yatima.Term            hiding (Let)

data Scheme where
  Define   :: Name     -> Scheme   -> Scheme
  Variable :: Name     -> Scheme
  Lambda   :: [Name]   -> Scheme   -> Scheme
  Apply    :: Scheme   -> [Scheme] -> Scheme
  Let      :: Name     -> Scheme   -> Scheme -> Scheme
  Letrec   :: Name     -> Scheme   -> Scheme -> Scheme
  Value    :: Literal  -> Scheme
  Operator :: PrimOp   -> Scheme
  Begin    :: [Scheme] -> Scheme   -> Scheme
  Assign   :: Name     -> Scheme   -> Scheme
  Equal    :: Scheme   -> Scheme   -> Scheme
  If       :: Scheme   -> Scheme   -> Scheme -> Scheme
  Nil      :: Scheme
  deriving (Show, Eq)

-- Transform an IR into Scheme AST.
--- All defined variables will start with ':' so to not collide with any primitive Scheme value or keyword
defToScheme :: Text -> IR -> Scheme
defToScheme nam trm = Define (T.cons ':' nam) (termToScheme Once trm)

termToScheme :: Uses -> IR -> Scheme
termToScheme use trm = go [] use trm where
  go args use trm = case trm of
    RefI nam -> apply args $ Variable (nonPrim nam)
    VarI nam -> apply args $ Variable (nonPrim nam)
    LamI argUse nam bod ->
      case argUse of
        None -> go args use bod
        _    -> apply args $ Lambda [nonPrim nam] (go [] use bod)
    AppI argUse fun arg ->
      case argUse of
        None -> go args use fun
        _    -> go (go [] (use *# argUse) arg : args) use fun
    LetI rec valUse nam val bod ->
      case valUse of
        None -> go args use bod
        _    -> apply args $ (if rec then Letrec else Let) (nonPrim nam) (go [] (use *# valUse) val) (go [] use bod)
    NewI exp            -> go args use exp
    UseI exp Nothing    -> go args use exp
    UseI exp (Just TNatural) -> let (cases, rest) = splitAt 2 args in
      -- inline cases $
      apply cases $
      Lambda ["zero_case"] $
      Lambda ["succ_case"] $
      Let "tmp" (go rest use exp) $
      If (Equal (Variable "tmp") (Value (VNatural 0))) (Variable "zero_case") $
      Apply (Variable "succ_case") [Apply (Operator Natural_pred) [Variable "tmp"]]
    UseI exp (Just typ) -> go args use exp -- TODO
    -- Native datatypes
    LitI lit -> noArgs args $ Value lit
    OprI opr ->
      let ari = oprArity opr
          (args', rest) = splitAt ari args
          vars = map (\n -> T.pack ("x" ++ show n)) [1 .. ari-length args']
          operands = args' ++ map Variable vars
      in noArgs rest $ foldr (\x -> Lambda [x]) (Apply (Operator opr) operands) vars
    -- Types are erased
    AllI nam use typ bod -> noArgs args Nil
    SlfI nam bod         -> noArgs args Nil
    TypI                 -> noArgs args Nil
    LTyI typ             -> noArgs args Nil
  nonPrim nam = T.cons ':' nam
  apply []         trm = trm
  apply (arg:args) trm = apply args (Apply trm [arg])
  noArgs args trm = if args == [] then trm else error "Compilation error"

--- Transform Scheme AST into code. The `Int` variable `dep` represents the depth of the indentation
schemeToCode :: Int -> Bool -> Scheme -> Text
schemeToCode dep ind trm = LT.toStrict $ TB.toLazyText (go dep ind trm) where
  go :: Int -> Bool -> Scheme -> TB.Builder
  go dep True  trm = (TB.fromString $ replicate dep ' ') <> go dep False trm
  go dep False trm = case trm of
    Define   nam  bod      -> "(define " <> name nam <> "\n" <> go 2 True bod <> ")\n\n"
    Variable nam           -> name nam
    Lambda   nams bod      -> "(lambda (" <> mergeWith " " (map name nams) <> ")\n" <> go (dep+2) True bod <> ")"
    Apply    fun  args     -> "(" <> go (dep+1) False fun <> "\n" <>
                              mergeWith "\n" (map (go (dep+1) True) args) <> ")"
    Let      nam  exp  bod -> "(let ((" <> name nam <> " " <>
                              go (dep+8+T.length nam) False exp <> "))\n" <>
                              go (dep+2) True bod <> ")"
    Letrec   nam  exp  bod -> "(letrec ((" <> name nam <> " " <>
                              go (dep+11+T.length nam) False exp <> "))\n" <>
                              go (dep+2) True bod <> ")"
    Value    lit           -> litToCode lit
    Operator opr           -> oprToCode opr
    Begin    exps end      -> "(begin\n" <> mergeWith "\n" (map (go (dep+2) True) (exps ++ [end])) <> ")"
    Assign   nam  exp      -> "(set! " <> name nam <> go (dep+7+T.length nam) False exp <> ")"
    Equal    exp1 exp2     -> "(= " <> go (dep+3) False exp1 <> "\n" <> go (dep+3) True exp2 <> ")"
    If       exp  tru  fal -> "(if " <> go (dep+4) False exp <> "\n" <> go (dep+4) True tru <> "\n" <> go (dep+4) True fal <> ")"
    Nil                    -> "'()"
  name :: Name -> TB.Builder
  name "" = "_"
  name x  = TB.fromText x
  mergeWith sep []     = ""
  mergeWith sep [x]    = x
  mergeWith sep (x:xs) = x <> sep <> mergeWith sep xs

defToCode :: Text -> IR -> Text
defToCode nam trm = schemeToCode 0 False $ defToScheme nam trm

litToCode :: Literal -> TB.Builder
litToCode lit = case lit of
  VNatural x -> TB.fromString $ show x
  _          -> "'()" -- TODO

oprToCode :: PrimOp  -> TB.Builder
oprToCode opr = case opr of
  Natural_succ -> "1+"
  Natural_pred -> "1-"
  Natural_add  -> "+"
  Natural_mul  -> "*"
  Natural_sub  -> "-"
  Natural_div  -> "div"
  Natural_mod  -> "mod"
  Natural_gt   -> ">"
  Natural_ge   -> ">="
  Natural_eq   -> "="
  Natural_ne   -> "(lambda (x y) (not (= x y)))"
  Natural_lt   -> "<"
  Natural_le   -> "<="
  _            -> "'()" -- TODO
