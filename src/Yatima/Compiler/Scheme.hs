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
import qualified Data.ByteString        as BS
import           Data.Map               (Map)
import qualified Data.Map               as Map

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
  apply args trm = foldl (\acc arg -> Apply acc [arg]) trm args
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
    Equal    exp1 exp2     -> "(equal? " <> go (dep+8) False exp1 <> go (dep+8) True exp2 <> ")"
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
  VNatural   x    -> show' x
  VI64       x    -> show' x
  VI32       x    -> show' x
  VF64       x    -> show' x
  VF32       x    -> show' x
  VBitVector n bs ->
    case BS.uncons bs of
      Nothing       -> "#vu8()"
      Just (c,cs)   -> "#vu8(" <> BS.foldl' (\cs c -> cs <> " " <> show' c) (show' c) cs <> ")"
  VString    t    -> show' t
  VChar      c    -> "#\\" <> TB.singleton c
  VException s    -> "(throw-error " <> TB.fromText s <> ")"
  VWorld          -> error "Cannot compile a world value."
  where
    show' :: Show a => a -> TB.Builder
    show' = TB.fromString . show

oprToCode :: PrimOp  -> TB.Builder
oprToCode opr = case opr of
  -- Natural number operations
  Natural_succ        -> "1+"
  Natural_pred        -> "pred"
  Natural_add         -> "+"
  Natural_mul         -> "*"
  Natural_sub         -> "-"
  Natural_div         -> "div"
  Natural_mod         -> "mod"
  Natural_gt          -> ">"
  Natural_ge          -> ">="
  Natural_eq          -> "="
  Natural_ne          -> "neq"
  Natural_lt          -> "<"
  Natural_le          -> "<="
  Natural_to_I64      -> error "TODO"
  Natural_to_I32      -> error "TODO"
  -- Char and string operations
  Char_to_U8          -> error "TODO"
  Char_ord            -> error "TODO"
  String_cons         -> "string-cons"
  String_concat       -> "string-append"
  -- Fixnum operations
  I64_clz             -> error "TODO"
  I64_ctz             -> error "TODO"
  I64_popcnt          -> error "TODO"
  I32_wrap_I64        -> error "TODO"
  F32_convert_I64_s   -> error "TODO"
  F32_convert_I64_u   -> error "TODO"
  F64_reinterpret_I64 -> error "TODO"
  F64_convert_I64_s   -> error "TODO"
  F64_convert_I64_u   -> error "TODO"
  Natural_from_I64    -> error "TODO"
  Char_chr            -> error "TODO"
  I64_to_U64          -> error "TODO"
  I64_eqz             -> "fxzero?"
  I64_eq              -> "fx="
  I64_ne              -> "fxneq"
  I64_lt_s            -> "fx<"
  I64_lt_u            -> error "TODO"
  I64_gt_s            -> "fx>"
  I64_gt_u            -> error "TODO"
  I64_le_s            -> "fx<="
  I64_le_u            -> error "TODO"
  I64_ge_s            -> "fx>="
  I64_ge_u            -> error "TODO"
  I64_add             -> "fx+"
  I64_sub             -> "fx-"
  I64_mul             -> "fx*"
  I64_div_s           -> "fxdiv"
  I64_div_u           -> error "TODO"
  I64_rem_s           -> "fxmod"
  I64_rem_u           -> error "TODO"
  I64_and             -> "fxand"
  I64_or              -> "fxior"
  I64_xor             -> "fxxor"
  I64_shl             -> "fxarithmetic-shift-left"
  I64_shr_s           -> "fxarithmetic-shift-right"
  I64_shr_u           -> error "TODO"
  I64_rotl            -> error "TODO"
  I64_rotr            -> error "TODO"
  I32_eqz             -> error "TODO"
  I32_clz             -> error "TODO"
  I32_ctz             -> error "TODO"
  I32_popcnt          -> error "TODO"
  I64_extend_I32_s    -> error "TODO"
  I64_extend_I32_u    -> error "TODO"
  F32_reinterpret_I32 -> error "TODO"
  F32_convert_I32_s   -> error "TODO"
  F32_convert_I32_u   -> error "TODO"
  F64_convert_I32_s   -> error "TODO"
  F64_convert_I32_u   -> error "TODO"
  Natural_from_I32    -> error "TODO"
  I32_to_U32          -> error "TODO"
  I32_eq              -> error "TODO"
  I32_ne              -> error "TODO"
  I32_lt_s            -> error "TODO"
  I32_lt_u            -> error "TODO"
  I32_gt_s            -> error "TODO"
  I32_gt_u            -> error "TODO"
  I32_le_s            -> error "TODO"
  I32_le_u            -> error "TODO"
  I32_ge_s            -> error "TODO"
  I32_ge_u            -> error "TODO"
  I32_add             -> error "TODO"
  I32_sub             -> error "TODO"
  I32_mul             -> error "TODO"
  I32_div_s           -> error "TODO"
  I32_div_u           -> error "TODO"
  I32_rem_s           -> error "TODO"
  I32_rem_u           -> error "TODO"
  I32_and             -> error "TODO"
  I32_or              -> error "TODO"
  I32_xor             -> error "TODO"
  I32_shl             -> error "TODO"
  I32_shr_u           -> error "TODO"
  I32_shr_s           -> error "TODO"
  I32_rotl            -> error "TODO"
  I32_rotr            -> error "TODO"
  -- Floating point operations
  F64_abs             -> error "TODO"
  F64_neg             -> error "TODO"
  F64_ceil            -> error "TODO"
  F64_floor           -> error "TODO"
  F64_trunc           -> error "TODO"
  F64_nearest         -> error "TODO"
  F64_sqrt            -> error "TODO"
  F64_promote_F32     -> error "TODO"
  I64_reinterpret_F64 -> error "TODO"
  F64_to_U64          -> error "TODO"
  I32_trunc_F64_s     -> error "TODO"
  I32_trunc_F64_u     -> error "TODO"
  I64_trunc_F64_s     -> error "TODO"
  I64_trunc_F64_u     -> error "TODO"
  F64_eq              -> error "TODO"
  F64_ne              -> error "TODO"
  F64_lt              -> error "TODO"
  F64_gt              -> error "TODO"
  F64_le              -> error "TODO"
  F64_ge              -> error "TODO"
  F64_add             -> error "TODO"
  F64_sub             -> error "TODO"
  F64_mul             -> error "TODO"
  F64_div             -> error "TODO"
  F64_min             -> error "TODO"
  F64_max             -> error "TODO"
  F64_copysign        -> error "TODO"
  F32_abs             -> error "TODO"
  F32_neg             -> error "TODO"
  F32_ceil            -> error "TODO"
  F32_floor           -> error "TODO"
  F32_trunc           -> error "TODO"
  F32_nearest         -> error "TODO"
  F32_sqrt            -> error "TODO"
  F32_demote_F64      -> error "TODO"
  I32_reinterpret_F32 -> error "TODO"
  I32_trunc_F32_s     -> error "TODO"
  I32_trunc_F32_u     -> error "TODO"
  I64_trunc_F32_s     -> error "TODO"
  I64_trunc_F32_u     -> error "TODO"
  F32_to_U32          -> error "TODO"
  F32_eq              -> error "TODO"
  F32_ne              -> error "TODO"
  F32_lt              -> error "TODO"
  F32_gt              -> error "TODO"
  F32_le              -> error "TODO"
  F32_ge              -> error "TODO"
  F32_add             -> error "TODO"
  F32_sub             -> error "TODO"
  F32_mul             -> error "TODO"
  F32_div             -> error "TODO"
  F32_min             -> error "TODO"
  F32_max             -> error "TODO"
  F32_copysign        -> error "TODO"
  -- Bitstring operations
  BitVector_b0        -> error "TODO"
  BitVector_b1        -> error "TODO"
  BitVector_length    -> error "TODO"
  BitVector_concat    -> error "TODO"
  Char_from_U8        -> error "TODO"
  I32_from_U32        -> error "TODO"
  F32_from_U32        -> error "TODO"
  I64_from_U64        -> error "TODO"
  F64_from_U64        -> error "TODO"

useLit :: LitType -> [Scheme] -> Scheme -> Scheme
useLit typ args trm = case typ of
  TNatural ->
    let bod z s = Apply (Variable "match-nat") [trm]
    in case args of
      []       -> enclose "case_zero" $ enclose "case_succ" . bod
      (x:[])   -> enclose "case_succ" $ bod x
      (x:y:xs) -> apply xs $ bod x y
  TString ->
    let bod z s = Apply (Variable "match-string") [trm]
    in case args of
      []       -> enclose "case_nil" $ enclose "case_cons" . bod
      (x:[])   -> enclose "case_cons" $ bod x
      (x:y:xs) -> apply xs $ bod x y
  TBitVector ->
    let bod z s = Apply (Variable "match-bitvector") [trm]
    in case args of
      []       -> enclose "case_nil" $ enclose "case_cons" . bod
      (x:[])   -> enclose "case_cons" $ bod x
      (x:y:xs) -> apply xs $ bod x y
  _ -> error "Use of non-inductive type in the compiler. Implementation is broken."
  where
    enclose nam bod = Lambda [nam] (bod (Variable nam))
    apply args trm = foldl (\acc arg -> Apply acc [arg]) trm args

auxiliaryDefs :: Map Text Text
auxiliaryDefs = Map.fromList $
  [ ("throw-error",
     "(define (throw-error msg)\n" <>
     "  (raise (condition (make-error) (make-message-condition msg))))")
  , ("neq", "(define (neq x y) (not (= x y)))")
  , ("fxneq", "(define (fxneq x y) (not (fx= x y)))")
  , ("pred", "(define (pred x) (if (= x 0) 0 (1- x)))")
  , ("string-cons",
     "(define (string-cons c cs)\n" <>
     "  (string-append (make-string 1 c) cs))")
  , ("match-string",
     "(define (match-string str n c)" <>
     "  (if (equal? str \"\")" <>
     "      n" <>
     "      (c (string-ref str 0)" <>
     "         (substring str 1 (string-length str)))))")
  , ("match-nat",
     "(define (match-nat n z s)" <>
     "  (if (equal? n 0) z (s (1- n))))")
  , ("bytevector-tail",
     "(define (bytevector-tail bs)" <>
     "  (let* ((n (1- (bytevector-length bs)))" <>
     "         (cs (make-bytevector n)))" <>
     "    (bytevector-copy! bs 1 cs 0 n)" <>
     "    cs))")
  ]
