{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Yatima.Parse.Term
-- Description : Parsing expressions in the Yatima Language
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This library implements a `Megaparsec` parser for the Yatima language using the
-- conventions specified in `Text.MegaParsec.Char.Lexer`. A helpful tutorial
-- explaining Megaparsec can be found on [Mark Karpov's
-- blog](https://markkarpov.com/tutorial/megaparsec.html)
-- |
module Yatima.Parse.Term where

import Control.Monad.RWS.Lazy hiding (All, Typ)
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T hiding (find)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Yatima.Parse.Literal
import Yatima.Parse.Parser
import Yatima.Term

pTerm :: (Ord e, Monad m) => Parser e m Term
pTerm = space >> (pExpr True)

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence may be annotated with a type
pExpr :: (Ord e, Monad m) => Bool -> Parser e m Term
pExpr annotatable = label "an expression" $ do
  from <- getSourcePos
  fun <- pTerm' <* space
  args <- args
  let tele = foldl (\t (upto, a) -> App (mkLoc from upto) t a) fun args
  let ann = do
        ty <- symbol "::" >> pExpr False
        upto <- getSourcePos
        return $ Ann (mkLoc from upto) tele ty
  choice [if annotatable then ann else empty, return tele]
  where
    args = next <|> (return [])
    next = do
      notFollowedBy terminator
      l <- getSourcePos
      t <- pTerm' <* space
      ts <- args
      return ((l, t) : ts)
    terminator =
      choice
        [ void (string "def"),
          void (string "::"),
          void (string "{|"),
          void eof
        ]

-- | Parse an inner term
pTerm' :: (Ord e, Monad m) => Parser e m Term
pTerm' = do
  choice
    [ pLam,
      pAll,
      pSlf,
      pUse,
      pNew,
      pTyp,
      symbol "(" >> pExpr True <* space <* string ")",
      pLet,
      do
        from <- getSourcePos
        o <- pOpr
        upto <- getSourcePos
        return $ Opr (mkLoc from upto) o,
      do
        from <- getSourcePos
        o <- pLitType
        upto <- getSourcePos
        return $ LTy (mkLoc from upto) o,
      do
        from <- getSourcePos
        o <- pLiteral
        upto <- getSourcePos
        return $ Lit (mkLoc from upto) o,
      pVar
    ]

pName :: (Ord e, Monad m) => Bool -> Parser e m Text
pName bind = label "a name: \"someFunc\",\"somFunc'\",\"x_15\", \"_1\"" $ do
  n <- alphaNumChar <|> oneOf nameSymbol
  ns <- many (alphaNumChar <|> oneOf nameSymbol)
  let nam = T.pack (n : ns)
  if
      | isDigit n -> customFailure $ LeadingDigit nam
      | isReservedLead n -> customFailure $ ReservedLeadingChar n nam
      | nam `Set.member` keywords -> customFailure $ ReservedKeyword nam
      | otherwise -> return nam
  where
    isReservedLead n = n `elem` ("'-" :: [Char])
    syms = "_'-" :: [Char]
    nameSymbol = if bind then syms else syms ++ "."

keywords :: Set Text
keywords =
  Set.fromList $
    [ "let",
      "def",
      "Type"
    ]

-- | Parse a quantitative usage semirig annotation. The absence of annotation is
-- considered to be the `Many` multiplicity.
pUses :: (Ord e, Monad m) => Parser e m Uses
pUses = pUsesAnnotation <|> return Many

pUsesAnnotation :: (Ord e, Monad m) => Parser e m Uses
pUsesAnnotation =
  choice
    [ symbol "0" >> return None,
      symbol "&" >> return Affi,
      symbol "1" >> return Once
    ]

-- | Parse the type of types: @Type@
pTyp :: (Ord e, Monad m) => Parser e m Term
pTyp = label "a type: \"Type\"" $ do
  from <- getSourcePos
  string "Type"
  upto <- getSourcePos
  return $ Typ (mkLoc from upto)

pBinder :: (Ord e, Monad m) => Bool -> Parser e m [(SourcePos, Name, Uses, Term)]
pBinder namOptional =
  choice
    [ try $ ann,
      if namOptional then unNam else empty
    ]
  where
    unNam = (\l x -> [(l, "", Many, x)]) <$> getSourcePos <*> pTerm'
    ann = do
      symbol "("
      uses <- pUses
      names <- sepEndBy1 ((,) <$> getSourcePos <*> pName True) space
      typ_ <- symbol ":" >> pExpr False
      string ")"
      return $ zipWith (\(l, n) i -> (l, n, uses, shift i 0 typ_)) names [0 ..]

foldLam :: SourcePos -> Term -> [(SourcePos, Name)] -> Term
foldLam upto body bs = foldr (\(f, n) x -> Lam (mkLoc f upto) n x) body bs

-- | Parse a lambda: @λ x y z => body@
pLam :: (Ord e, Monad m) => Parser e m Term
pLam = label "a lambda: \"λ x y => y\"" $ do
  symbol "λ" <|> symbol "\\" <|> symbol "lambda"
  vars <- sepEndBy1 ((,) <$> getSourcePos <*> pName True) space
  symbol "=>"
  body <- bind (snd <$> vars) (pExpr False)
  upto <- getSourcePos
  return (foldLam upto body vars)

foldAll :: SourcePos -> Term -> [(SourcePos, Name, Uses, Term)] -> Term
foldAll upto body bs =
  foldr (\(from, n, u, t) x -> All (mkLoc from upto) n u t x) body bs

bindAll ::
  (Ord e, Monad m) =>
  [(SourcePos, Name, Uses, Term)] ->
  Parser e m a ->
  Parser e m a
bindAll bs = bind (foldr (\(_, n, _, _) ns -> n : ns) [] bs)

-- | Parse a forall: ∀ (a: A) (b: B) (c: C) -> body@
pAll :: (Ord e, Monad m) => Parser e m Term
pAll = label "a forall: \"∀ (a: A) (b: B) -> A\"" $ do
  symbol "∀" <|> symbol "forall"
  binds <- binders <* space
  body <- bindAll binds (pExpr False)
  upto <- getSourcePos
  return $ foldAll upto body binds
  where
    binder = pBinder True
    binders = do
      b <- binder <* space
      bs <- bindAll b $ ((symbol "->" >> return []) <|> binders)
      return $ b ++ bs

pSlf :: (Ord e, Monad m) => Parser e m Term
pSlf = label "a self type: \"@x A\"" $ do
  from <- getSourcePos
  name <- symbol "@" >> (pName True) <* space
  body <- bind [name] $ pExpr False
  upto <- getSourcePos
  return $ Slf (mkLoc from upto) name body

pNew :: (Ord e, Monad m) => Parser e m Term
pNew = label "datatype introduction: \"data x\"" $ do
  from <- getSourcePos
  symbol "data "
  expr <- pExpr True
  upto <- getSourcePos
  return $ New (mkLoc from upto) expr

pUse :: (Ord e, Monad m) => Parser e m Term
pUse = label "a case expression: \"case x\"" $ do
  from <- getSourcePos
  symbol "case "
  expr <- pExpr True
  upto <- getSourcePos
  return $ Use (mkLoc from upto) expr

pDecl :: (Ord e, Monad m) => Bool -> Bool -> Parser e m (Name, Term, Term)
pDecl rec shadow = do
  nam <- (pName True) <* space
  refs <- asks _refs
  when
    (not shadow && M.member nam refs)
    (customFailure $ TopLevelRedefinition nam)
  bs <- ((symbol ":" >> return []) <|> binders)
  let ns = (snd4 <$> bs)
  typBody <- bind ns (pExpr False)
  expBody <- symbol "=" >> bind (if rec then (nam : ns) else ns) (pExpr False)
  upto <- getSourcePos
  let exp = foldLam upto expBody ((\(n, l, _, _) -> (n, l)) <$> bs)
  let typ = foldAll upto typBody bs
  return (nam, exp, typ)
  where
    snd4 (_, x, _, _) = x
    binder = pBinder False
    binders = do
      b <- binder <* space
      bs <- bind (snd4 <$> b) $ ((symbol ":" >> return []) <|> binders)
      return $ b ++ bs

-- | Parse a local, possibly recursive, definition
pLet :: (Ord e, Monad m) => Parser e m Term
pLet = do
  from <- getSourcePos
  rec <- (symbol "letrec" >> return True) <|> (symbol "let" >> return False)
  use <- pUses
  (nam, exp, typ) <- pDecl rec True <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  upto <- getSourcePos
  return $ Let (mkLoc from upto) rec nam use typ exp bdy

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: (Ord e, Monad m) => Parser e m Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  from <- getSourcePos
  env <- ask
  nam <- pName False
  upto <- getSourcePos
  case findByName nam (_context env) of
    Just i -> return $ Var (mkLoc from upto) nam i
    Nothing -> case M.lookup nam (_refs env) of
      Just (def, trm) -> return (Ref (mkLoc from upto) nam def trm)
      Nothing -> customFailure $ UndefinedReference nam

pAnn :: (Ord e, Monad m) => Parser e m Term
pAnn = do
  from <- getSourcePos
  symbol "("
  val <- pExpr False <* space
  symbol "::"
  typ <- pExpr False <* space
  string ")"
  upto <- getSourcePos
  return $ Ann (mkLoc from upto) val typ
