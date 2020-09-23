{-|
Module      : Language.Yatima.HOAS
Description : Evaluate and typecheck exprassions in the Yatima Language using
higher-order-abstract-syntax
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
module Language.Yatima.HOAS
  ( HOAS(..)
  , Error(..)
  , findCtx
  , termToHoas
  , hoasToTerm
  , whnf
  , norm
  , infer
  , check
  , synth
  , fill
  , prettyHoas
  , prettyError
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Set as Set
import Language.Yatima.Print
import Language.Yatima.Term

import Debug.Trace

-- | Higher-Order Abstract Syntax
data HOAS where
  AnyH :: HOAS
  HolH :: Name -> HOAS
  VarH :: Int -> HOAS
  AllH :: Name -> HOAS -> (HOAS -> HOAS) -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS

-- | A type error
data Error
  = TypeMismatch Context HOAS HOAS
  | UnboundVariable
  | FoundHole Hole
  | NonFunctionApplication
  | NonFunctionLambda
  | UntypedLambda

-- | A type context
type Context = [(Name, HOAS)]

-- | A filled hole
type Hole = (Name, HOAS)

-- Utils
-- =====

-- | Find a term in a context
findCtx :: Name -> Context -> Maybe HOAS
findCtx findName ctx = go ctx where
  go ((name,term):ctx) | name == findName = Just term
                       | otherwise        = go ctx
  go []                                   = Nothing

-- | Gets the term at given Bruijn-level in a context
atCtx :: Int -> Context -> Maybe (Name, HOAS)
atCtx lvl ctx = go (length ctx - lvl - 1) ctx where
  go :: Int -> Context -> Maybe (Name, HOAS)
  go 0 (x : xs) = Just x
  go i (x : xs) = go (i - 1) xs
  go i []       = Nothing

-- Conversions
-- ===========

-- | Convert a lower-order `Term` to a GHC higher-order one
termToHoas :: Context -> Term -> HOAS
termToHoas ctx term = case term of
  Any                -> AnyH
  Hol name           -> HolH name
  Var name           -> maybe (VarH 0) id (findCtx name ctx)
  All name bind body -> AllH name (termToHoas ctx bind) (\x -> termToHoas ((name,x):ctx) body) 
  Lam name body      -> LamH name (\x -> termToHoas ((name,x):ctx) body)
  App func argm      -> AppH (termToHoas ctx func) (termToHoas ctx argm)

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: Context -> HOAS -> Term
hoasToTerm ctx term = case term of
  AnyH                -> Any
  HolH name           -> Hol name
  VarH indx           -> Var (maybe (Text.concat ["x", Text.pack (show (length ctx - indx - 1))]) fst (atCtx indx ctx))
  AllH name bind body -> All name (hoasToTerm ctx bind) (hoasToTerm ((name,AnyH):ctx) (body (VarH (length ctx))))
  LamH name body      -> Lam name (hoasToTerm ((name,AnyH):ctx) (body (VarH (length ctx))))
  AppH func argm      -> App (hoasToTerm ctx func) (hoasToTerm ctx argm)

-- Normalization
-- =============

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> HOAS
whnf term = case term of
  AppH func argm -> case whnf func of
    LamH _ body -> whnf (body argm)
    otherwise   -> term
  term -> term

-- | Normalize a HOAS term
norm :: HOAS -> HOAS
norm term = case whnf term of
  AllH name bind body -> AllH name (norm bind) (\ x -> norm (body x))
  LamH name body      -> LamH name (\ x -> norm (body x))
  AppH func argm      -> AppH (norm func) (norm argm)
  term                -> term

-- Type System
-- ===========

-- | Infers the type of a term
infer :: Context -> HOAS -> Either Error HOAS
infer ctx term = trace ("... infer " ++ Text.unpack (prettyHoas ctx term)) (go ctx term) where
  go ctx AnyH = do
    return AnyH
  go ctx (VarH lvl) = case atCtx lvl ctx of
    Nothing -> Left UnboundVariable
    Just (nam,typ) -> return typ
  go ctx termTyp@(AllH name bind body) = do
    let lvl = length ctx
    check ctx bind AnyH
    check ((name,bind) : ctx) (body (VarH lvl)) AnyH
    return AnyH
  go ctx (LamH name body) = do
    trace ("..." ++ Text.unpack name) $
      Left UntypedLambda
  go ctx (AppH func argm) = do
    funcTyp <- go ctx func
    case whnf funcTyp of
      AllH name bind body -> do
        check ctx argm bind
        return (body argm)
      otherwise -> Left NonFunctionApplication
  go ctx (HolH name) = do
    return AnyH

-- | Checks if a term has given type
check :: Context -> HOAS -> HOAS -> Either Error HOAS
check ctx term tipo =
  trace ("... check " ++ Text.unpack (prettyHoas ctx term) ++ " :: " ++ Text.unpack (prettyHoas ctx tipo)) $
    let lvl = length ctx in
    let tipoNf = whnf tipo in
    case term of
      (LamH lamName lamBody) ->
        case tipoNf of
          (AllH allName allBind allBody) ->
            let bodyTrm = lamBody (VarH lvl) in
            let bodyTyp = allBody (VarH lvl) in
            let bodyCtx = (lamName,allBind) : ctx in
            do check bodyCtx bodyTrm bodyTyp
               return tipo
          otherwise -> Left NonFunctionLambda
      otherwise -> do
        termTyp <- infer ctx term
        trace ("... equal " ++ Text.unpack (prettyHoas ctx tipo) ++ " == " ++ Text.unpack (prettyHoas ctx termTyp)) $
          case equal tipo termTyp lvl of
            Left hole   -> trace ("... fills " ++ Text.unpack (fst hole) ++ " = " ++ Text.unpack (prettyHoas ctx (snd hole))) $ Left (FoundHole hole)
            Right False -> Left (TypeMismatch ctx tipo termTyp)
            Right True  -> return tipo

-- Equality
-- ========

-- Converts a term to a unique string representation. This is used by equal.
-- TODO: use a hash function instead
serialize :: Int -> HOAS -> String
serialize lvl term = go term lvl lvl "" where
  go :: HOAS -> Int -> Int -> String -> String
  go AnyH                  lvl ini x              = '*' : x
  go (VarH n)              lvl ini x | lvl >= ini = '^' : show (lvl-n-1) ++ x
                                     | otherwise  = '#' : show n ++ x
  go (AllH name bind body) lvl ini x              = '∀' : Text.unpack name ++ go bind lvl ini (go (body (VarH lvl)) (lvl+1) ini x)
  go (LamH name body)      lvl ini x              = 'λ' : Text.unpack name ++ go (body (VarH lvl)) (lvl+1) ini x
  go (AppH func argm)      lvl ini x              = '@' : go func lvl ini (go argm lvl ini x)
  go (HolH name)           lvl ini x              = '?' : Text.unpack name ++ x

-- | Checks if two terms are equal
equal :: HOAS -> HOAS -> Int -> Either Hole Bool
equal a b lvl = go a b lvl Set.empty where
  go a b lvl seen = do
    let an = whnf a
    let bn = whnf b
    let ah = serialize lvl an
    let bh = serialize lvl bn
    let id = ah ++ "==" ++ bh
    let sn = Set.insert id seen
    if ah == bh || Set.member id seen then
      return True
    else case (an, bn) of
      (AllH anName anBind anBody, AllH bnName bnBind bnBody) -> do
        let anBodyVal = anBody (VarH lvl)
        let bnBodyVal = bnBody (VarH lvl)
        eqBind <- go anBind bnBind lvl sn
        eqBody <- go anBodyVal bnBodyVal (lvl+1) sn
        return (eqBind && eqBody)
      (LamH anName anBody, LamH bnName bnBody) -> do
        let anBodyVal = anBody (VarH lvl)
        let bnBodyVal = bnBody (VarH lvl)
        eqBody <- go anBodyVal bnBodyVal (lvl+1) sn
        return eqBody
      (AppH anFunc anArgm, AppH bnFunc bnArgm) ->  do
        eqFunc <- go anFunc bnFunc lvl sn
        eqArgm <- go anArgm bnArgm lvl sn
        return (eqFunc && eqArgm)
      (HolH anName, b) -> Left (anName, b)
      (a, HolH bnName) -> Left (bnName, a)
      (AnyH, b) -> return True
      (a, AnyH) -> return True
      otherwise -> return False

-- | Fills holes of a term until it is complete
synth :: HOAS -> HOAS -> Either Error (HOAS, HOAS)
synth term tipo = do
  case check [] term tipo of
    Left (FoundHole hole) -> synth (fill hole term) (fill hole tipo)
    Left error            -> Left error
    Right tipo            -> return (term, tipo)

-- | Fills a single hole in a term
fill :: Hole -> HOAS -> HOAS
fill hole AnyH                  = AnyH
fill hole (VarH name)           = VarH name
fill hole (LamH name body)      = LamH name (\x -> fill hole (body x))
fill hole (AllH name bind body) = AllH name (fill hole bind) (\x -> fill hole (body x))
fill hole (AppH func argm)      = AppH (fill hole func) (fill hole argm)
fill hole (HolH name)           = if fst hole == name then snd hole else HolH name

-- Printing
-- ========

-- | Pretty-print a `HOAS`
prettyHoas :: Context -> HOAS -> Text
prettyHoas ctx term = prettyTerm (hoasToTerm ctx term)

instance Show HOAS where
  show term = Text.unpack (prettyHoas [] term)

-- Stringifies a type error
prettyError :: Error -> Text
prettyError (TypeMismatch ctx a b) =
  let aStr = prettyHoas ctx a in
  let bStr = prettyHoas ctx b in
  Text.concat [
    "Type mismatch.",
    "\nFound type... ", aStr,
    "\nInstead of... ", bStr]
prettyError UnboundVariable =
  "Unbound variable."
prettyError (FoundHole (name, term)) =
  Text.concat ["Unfilled hole: ", name, "."]
prettyError NonFunctionApplication =
  "Non-function application."
prettyError NonFunctionLambda =
  "Non-function lambda."
prettyError UntypedLambda =
  "Non-annotated lambda."
