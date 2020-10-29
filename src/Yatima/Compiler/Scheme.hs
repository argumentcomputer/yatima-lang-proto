{-|
  Moduel      : Data.Compiler.Scheme
  Copyright   : 2020 Yatima Inc.
  Description : A compiler from Yatima to R5RS Scheme
  License     : GPL-3
  Maintainer  : gabriel@yatima.io
  Stability   : experimental
-}
module Yatima.Compiler.Scheme where

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TB

import           Yatima.Core.IR
import           Yatima.Core.Prim
import           Yatima.Term

data Scheme where
  Define   :: Name     -> Scheme   -> Scheme
  Variable :: Name     -> Scheme
  Lambda   :: [Name]   -> Scheme   -> Scheme
  Apply    :: Scheme   -> [Scheme] -> Scheme
  Lets     :: [(Name,     Scheme)] -> Scheme -> Scheme
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
    -- Erased computations
    LamI None nam bod   -> go args use bod
    AppI None fun _     -> go args use fun
    LetI _ None _ _ bod -> go args use bod

    RefI nam -> apply args $ Variable (nonPrim nam)
    VarI nam -> apply args $ Variable (nonPrim nam)
    LamI argUse nam bod ->
      apply args $ Lambda [nonPrim nam] (go [] use bod)
    AppI argUse fun arg ->
      go (go [] (use *# argUse) arg : args) use fun
    LetI False valUse nam val bod ->
      apply args $ captureLet use trm []
    LetI True  valUse nam val bod ->
      apply args $ Letrec (nonPrim nam) (go [] (use *# valUse) val) (go [] use bod)
    NewI exp            -> go args use exp
    UseI exp Nothing    -> go args use exp
    UseI exp (Just typ) -> useLit typ args (go [] use exp)
    -- Native datatypes
    LitI lit -> noArgs args $ Value lit
    OprI opr ->
      let arity = oprArity opr
          (appArgs, restArgs) = splitAt arity args
          -- Rest of the lambda variables that were not annihilited by the application of appArgs:
          restVars = map (\n -> T.pack ("x" ++ show n)) [1 .. arity-length appArgs]
          operands = appArgs ++ map Variable restVars
      in noArgs restArgs $ foldr (\x -> Lambda [x]) (Apply (Operator opr) operands) restVars
    -- Types are erased
    AllI nam use typ bod -> noArgs args Nil
    SlfI nam bod         -> noArgs args Nil
    TypI                 -> noArgs args Nil
    LTyI typ             -> noArgs args Nil
  nonPrim nam = T.cons ':' nam
  apply []         trm = trm
  apply (arg:args) trm = apply args (Apply trm [arg])
  noArgs args trm = if args == [] then trm else error "Compilation error"
  captureLet use trm lets = case trm of
    AppI None fun _     -> captureLet use fun lets
    LamI None _ bod     -> captureLet use bod lets
    LetI _ None _ _ bod -> captureLet use bod lets
    LetI False valUse nam val bod ->
      captureLet use bod ((nonPrim nam, go [] (use *# valUse) val) : lets)
    _ -> Lets (reverse lets) (go [] use trm)

-- Transform Scheme AST into code.
schemeToCode :: Scheme -> Text
schemeToCode trm = LT.toStrict $ TB.toLazyText (go 0 False trm) where
  -- The `Int` variable `dep` represents the depth of the indentation
  go :: Int -> Bool -> Scheme -> TB.Builder
  go dep True  trm = indent dep <> go dep False trm
  go dep False trm = case trm of
    Define   nam  bod      -> "(define " <> name nam <> go 2 True bod <> ")\n"
    Variable nam           -> name nam
    Lambda   nams bod      -> "(lambda (" <> mergeWith " " (map name nams) <> ")" <> go (dep+2) True bod <> ")"
    Apply    fun  args     ->
      "(" <> go (dep+1) False fun <>
      mconcat (map (go (dep+1) True) args) <> ")"
    Lets     bnds bod      ->
      "(let* (" <>
      mergeWith (indent (dep+7)) (map printBnd bnds) <>
      ")" <> go (dep+2) True bod <> ")"
      where printBnd (nam, exp) = "(" <> name nam <> " " <> go (dep+9+T.length nam) False exp <> ")"
    Letrec   nam  exp  bod ->
      "(letrec ((" <> name nam <> " " <>
      go (dep+11+T.length nam) False exp <> "))" <>
      go (dep+2) True bod <> ")"
    Value    lit           -> litToCode lit
    Operator opr           -> oprToCode opr
    Begin    exps end      -> "(begin" <> mconcat (map (go (dep+2) True) (exps ++ [end])) <> ")"
    Assign   nam  exp      -> "(set! " <> name nam <> go (dep+7+T.length nam) False exp <> ")"
    Equal    exp1 exp2     -> "(= " <> go (dep+3) False exp1 <> go (dep+3) True exp2 <> ")"
    If       exp  tru  fal -> "(if " <> go (dep+4) False exp <> go (dep+4) True tru <> go (dep+4) True fal <> ")"
    Nil                    -> "'()"
  name :: Name -> TB.Builder
  name "" = "_"
  name x  = TB.fromText x
  indent dep = "\n" <> (TB.fromString $ replicate dep ' ')
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
    let bod z s = Lets [("tmp", trm)] $
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
