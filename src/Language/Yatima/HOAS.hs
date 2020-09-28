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
import Language.Yatima.Uses
import Language.Yatima.Print
import Language.Yatima.Term

import Debug.Trace

-- | Higher-Order Abstract Syntax
data HOAS where
  AnyH :: HOAS
  HolH :: Name -> HOAS
  VarH :: Int -> HOAS
  AllH :: Name -> Uses -> HOAS -> (HOAS -> HOAS) -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  AnnH :: HOAS -> HOAS -> HOAS

-- | A type error
data Error
  = TypeMismatch PreContextH HOAS HOAS
  | UnboundVariable
  | FoundHole Hole
  | NonFunctionApplication
  | NonFunctionLambda
  | UntypedLambda
  | QuantityMismatch Uses Uses
  | ImpossibleCase

-- | A generalized context
type Context a = [(Name, a)]

-- Context utils
-- =====
-- | Find a term in a context
findCtx :: Name -> Context a -> Maybe a
findCtx findName ctx = go ctx where
  go ((name,a):ctx) | name == findName = Just a
                       | otherwise        = go ctx
  go []                                   = Nothing

-- | Gets the term at given Bruijn-level in a context
atCtx :: Int -> Context a -> Maybe (Name, a)
atCtx lvl ctx = go (length ctx - lvl - 1) ctx where
  go :: Int -> Context a -> Maybe (Name, a)
  go 0 (x : xs) = Just x
  go i (x : xs) = go (i - 1) xs
  go i []       = Nothing

-- | Modifies the context at a single place and returns the old value
modifyAt :: Int -> Context a -> (a -> a) -> Maybe (a, Context a)
modifyAt idx []                 f = Nothing
modifyAt idx ((name, a) : ctx) f =
  if idx > 0
  then do
    (a', ctx') <- modifyAt (idx - 1) ctx f
    return (a', (name, a) : ctx')
  else Just (a, (name, f a) : ctx)

-- | Typing context
type PreContextH = Context HOAS
type ContextH    = Context (Uses, HOAS)

toCtx :: PreContextH -> ContextH
toCtx = fmap $ \(name, term) -> (name, (None, term))

multiplyCtx :: Uses -> ContextH -> ContextH
multiplyCtx uses ctx = if uses == Once then ctx else fmap mul ctx
  where mul (name, (uses', typ)) = (name, (uses *# uses', typ))

-- Assumes both context are compatible
addCtx :: ContextH -> ContextH -> ContextH
addCtx = zipWith add
  where add (name, (uses,typ)) (_, (uses',_)) = (name, (uses +# uses', typ))

-- | A filled hole
type Hole = (Name, HOAS)

-- Conversions
-- ===========

-- | Convert a lower-order `Term` to a GHC higher-order one
termToHoas :: PreContextH -> Term -> HOAS
termToHoas ctx term = case term of
  Any                     -> AnyH
  Hol name                -> HolH name
  Var name                -> maybe (VarH 0) id (findCtx name ctx)
  All name uses bind body -> AllH name uses (termToHoas ctx bind) (\x -> termToHoas ((name,x):ctx) body)
  Lam name body           -> LamH name (\x -> termToHoas ((name,x):ctx) body)
  App func argm           -> AppH (termToHoas ctx func) (termToHoas ctx argm)

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: PreContextH -> HOAS -> Term
hoasToTerm ctx term = case term of
  AnyH                     -> Any
  HolH name                -> Hol name
  VarH indx                -> Var (maybe (Text.concat ["x", Text.pack (show (length ctx - indx - 1))]) fst (atCtx indx ctx))
  AllH name uses bind body -> All name uses (hoasToTerm ctx bind) (hoasToTerm ((name,AnyH):ctx) (body (VarH (length ctx))))
  LamH name body           -> Lam name (hoasToTerm ((name,AnyH):ctx) (body (VarH (length ctx))))
  AppH func argm           -> App (hoasToTerm ctx func) (hoasToTerm ctx argm)

-- -- Normalization
-- -- =============

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
  AllH name uses bind body -> AllH name uses (norm bind) (\ x -> norm (body x))
  LamH name body           -> LamH name (\ x -> norm (body x))
  AppH func argm           -> AppH (norm func) (norm argm)
  term                     -> term

-- Type System
-- ===========

-- | Infers the type of a term
infer :: PreContextH -> Uses -> HOAS -> Either Error (ContextH, HOAS)
infer pre uses term = trace ("... infer " ++ Text.unpack (prettyHoas pre term)) (go pre uses term) where
  -- Term rules
  go pre uses (VarH lvl) = case modifyAt lvl (toCtx pre) (\(_,typ) -> (uses,typ)) of
    Nothing -> Left UnboundVariable
    Just ((_,typ),ctx) -> return (ctx, typ)
  go pre uses (AppH func argm) = do
    (ctx, funcTyp) <- go pre uses func
    case whnf funcTyp of
      AllH _ argmUses bind body -> do
        (ctx',_) <- check pre (argmUses *# uses) argm bind
        return (addCtx ctx ctx', body argm)
      otherwise -> Left NonFunctionApplication
  go pre uses (LamH name body) = do
    trace ("..." ++ Text.unpack name) $
      Left UntypedLambda
  -- Typing rules (multiplicities are irrelevant)
  go pre _ AnyH = do
    return (toCtx pre, AnyH)
  go pre _ termTyp@(AllH name uses bind body) = do
    let lvl = length pre
    check pre None bind AnyH
    check ((name,bind) : pre) None (body (VarH lvl)) AnyH
    return (toCtx pre, AnyH)
  -- Hole
  go pre _ (HolH name) = do
    return (toCtx pre, AnyH)

-- | Checks if a term has given type
check :: PreContextH -> Uses -> HOAS -> HOAS -> Either Error (ContextH, HOAS)
check pre uses term tipo =
  trace ("... check " ++ Text.unpack (prettyHoas pre term) ++ " :: " ++ Text.unpack (prettyHoas pre tipo)) $ do
  let lvl = length pre
  let tipoNf = whnf tipo
  case term of
    (LamH lamName lamBody) ->
      case tipoNf of
        (AllH allName allUses allBind allBody) -> do
          let bodyTrm = lamBody (VarH lvl)
          let bodyTyp = allBody (VarH lvl)
          let extPre = (lamName,allBind) : pre
          (extCtx,_) <- check extPre uses bodyTrm bodyTyp
          case extCtx of
            [] -> Left ImpossibleCase
            (_,(lamUses,_)) : ctx -> do
              if lamUses ≤# allUses
                then Left (QuantityMismatch lamUses allUses)
                else return (multiplyCtx uses ctx, tipo)
        otherwise -> Left NonFunctionLambda
    otherwise -> do
      (ctx, termTyp) <- infer pre uses term
      trace ("... equal " ++ Text.unpack (prettyHoas pre tipo) ++ " == " ++ Text.unpack (prettyHoas pre termTyp)) $
        case equal tipo termTyp lvl of
          Left hole   -> trace ("... fills " ++ Text.unpack (fst hole) ++ " = " ++ Text.unpack (prettyHoas pre (snd hole))) $ Left (FoundHole hole)
          Right False -> Left (TypeMismatch pre tipo termTyp)
          Right True  -> return (ctx, tipo)

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
  go (AllH name uses bind body) lvl ini x              = '∀' : Text.unpack name ++ go bind lvl ini (go (body (VarH lvl)) (lvl+1) ini x)
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
      (AllH anName anUses anBind anBody, AllH bnName bnUses bnBind bnBody) -> do
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
  case check [] Once term tipo of
    Left (FoundHole hole) -> synth (fill hole term) (fill hole tipo)
    Left error            -> Left error
    Right (_,tipo)        -> return (term, tipo)

-- | Fills a single hole in a term
fill :: Hole -> HOAS -> HOAS
fill hole AnyH                       = AnyH
fill hole (VarH name)                = VarH name
fill hole (LamH name body)           = LamH name (\x -> fill hole (body x))
fill hole (AllH name uses bind body) = AllH name uses (fill hole bind) (\x -> fill hole (body x))
fill hole (AppH func argm     )      = AppH (fill hole func) (fill hole argm)
fill hole (HolH name)                = if fst hole == name then snd hole else HolH name

-- Printing
-- ========

-- | Pretty-print a `HOAS`
prettyHoas :: PreContextH -> HOAS -> Text
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
