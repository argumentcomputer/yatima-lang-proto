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

defToCode :: Text -> IR -> Text
defToCode nam trm = schemeToCode $ defToScheme nam trm

-- Transform an IR into Scheme AST.
--- All defined variables will start with ':' so to not collide with any primitive Scheme value or keyword
defToScheme :: Text -> IR -> Scheme
defToScheme nam trm = Define (T.cons ':' nam) (termToScheme trm)

termToScheme :: IR -> Scheme
termToScheme trm = go [] Once trm where
  go :: [Scheme] -> Uses -> IR -> Scheme
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
    UseI exp (Just typ) -> useLit typ args (go [] use exp)
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

-- Transform Scheme AST into code.
schemeToCode :: Scheme -> Text
schemeToCode trm = LT.toStrict $ TB.toLazyText (go 0 False trm) where
  -- The `Int` variable `dep` represents the depth of the indentation
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

useLit :: LitType -> [Scheme] -> Scheme -> Scheme
useLit typ args trm = case typ of
  TNatural ->
    let bod z s = Let "tmp" trm $
                  If (Equal (Variable "tmp") (Value (VNatural 0))) z $
                  Apply s [Apply (Operator Natural_pred) [Variable "tmp"]]
    in case args of
      []       -> enclose "case_zero" $ enclose "case_succ" . bod
      (x:[])   -> enclose "case_succ" $ bod x
      (x:y:xs) -> apply xs $ bod x y
  _ -> trm -- TODO
  where
    enclose nam bod = Lambda [nam] (bod (Variable nam))
    apply []         trm = trm
    apply (arg:args) trm = apply args (Apply trm [arg])
