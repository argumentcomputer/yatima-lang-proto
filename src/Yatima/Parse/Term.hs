{-|
Module      : Yatima.Parse.Term
Description : Parsing expressions in the Yatima Language
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental

This library implements a `Megaparsec` parser for the Yatima language using the
conventions specified in `Text.MegaParsec.Char.Lexer`. A helpful tutorial
explaining Megaparsec can be found on [Mark Karpov's
blog](https://markkarpov.com/tutorial/megaparsec.html)
|-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
module Yatima.Parse.Term where

import           Control.Monad.RWS.Lazy     hiding (All, Typ)

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Char                  (isDigit)

import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.IPLD.CID
import           Yatima.Term
import           Yatima.Print
import           Yatima.Parse.Parser
import           Yatima.Parse.Literal

pTerm :: (Ord e, Monad m) => Parser e m Term
pTerm = space >> (pExpr True)

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence may be annotated with a type
pExpr :: (Ord e, Monad m) => Bool -> Parser e m Term
pExpr annotatable = label "an expression" $ do
  fun  <- pTerm' <* space
  args <- args
  let tele = foldl (\t a -> App t a) fun args
  choice
    [ if annotatable then (Ann tele <$> (symbol "::" >> pExpr False)) else empty
    , return tele
    ]
  where
    args = next <|> (return [])
    next = do
      notFollowedBy terminator
      t  <- pTerm' <* space
      ts <- args
      return (t:ts)
    terminator = choice
      [ void (string "def")
      , void (string "::")
      , void (string "{|")
      , void eof
      ]

-- | Parse an inner term
pTerm' :: (Ord e, Monad m) => Parser e m Term
pTerm' = do
  from <- getOffset
  choice
    [ pLam
    , pAll
    , pSlf
    , pUse
    , pNew
    , pTyp
    , symbol "(" >> pExpr True <* space <* string ")"
    , pLet
    , Opr <$> pOpr
    , LTy <$> pLitType
    , Lit <$> pLiteral
    , pVar
    ]


pName :: (Ord e, Monad m) => Bool -> Parser e m Text
pName bind = label "a name: \"someFunc\",\"somFunc'\",\"x_15\", \"_1\"" $ do
  n  <- alphaNumChar <|> oneOf nameSymbol
  ns <- many (alphaNumChar <|> oneOf nameSymbol)
  let nam = T.pack (n : ns)
  if | isDigit n                 -> customFailure $ LeadingDigit nam
     | isReservedLead n          -> customFailure $ ReservedLeadingChar n nam
     | nam `Set.member` keywords -> customFailure $ ReservedKeyword nam
     | otherwise -> return nam
  where
    isReservedLead n = n `elem` ("'-" :: [Char])
    syms             = "_'-" :: [Char]
    nameSymbol       = if bind then syms else syms ++ "/"

keywords :: Set Text
keywords = Set.fromList $
  [ "let"
  , "if"
  , "where"
  , "def"
  , "Type"
  ]

-- | Parse a quantitative usage semirig annotation. The absence of annotation is
-- considered to be the `Many` multiplicity.
pUses :: (Ord e, Monad m) => Parser e m Uses
pUses = pUsesAnnotation <|> return Many

pUsesAnnotation :: (Ord e, Monad m) => Parser e m Uses
pUsesAnnotation = choice
  [ symbol "0"       >> return None
  , symbol "&"       >> return Affi
  , symbol "1"       >> return Once
  ]

-- | Parse the type of types: @Type@
pTyp :: (Ord e, Monad m) => Parser e m Term
pTyp = label "a type: \"Type\"" $ do
  string "Type"
  return $ Typ

pBinder :: (Ord e, Monad m) => Bool -> Parser e m [(Name,Uses, Term)]
pBinder namOptional = choice
  [ try $ ann
  , if namOptional then unNam else empty
  ]
  where
    unNam = (\x -> [("", Many, x)]) <$> pTerm'
    ann = do
      symbol "("
      uses  <- pUses
      names <- sepEndBy1 (pName True) space
      typ_  <- symbol ":" >> pExpr False
      string ")"
      return $ (,uses,typ_) <$> names

foldLam:: Term -> [Name] -> Term
foldLam body bs = foldr (\n x -> Lam n x) body bs

-- | Parse a lambda: @λ x y z => body@
pLam :: (Ord e, Monad m) => Parser e m Term
pLam = label "a lambda: \"λ x y => y\"" $ do
  symbol "λ" <|> symbol "\\" <|> symbol "lambda"
  vars <- sepEndBy1 (pName True) space
  symbol "=>"
  body <- bind vars (pExpr False)
  return (foldLam body vars)

foldAll :: Term -> [(Name, Uses, Term)] -> Term
foldAll body bs = foldr (\(n,u,t) x -> All n u t x) body bs

bindAll :: (Ord e, Monad m) => [(Name,Uses,Term)] -> Parser e m a -> Parser e m a
bindAll bs = bind (foldr (\(n,_,_) ns -> n:ns) [] bs)

fst3 (x,y,z) = x

-- | Parse a forall: ∀ (a: A) (b: B) (c: C) -> body@
pAll :: (Ord e, Monad m) => Parser e m Term
pAll = label "a forall: \"∀ (a: A) (b: B) -> A\"" $ do
  symbol "∀" <|> symbol "forall"
  binds <- binders <* space
  body  <- bindAll binds (pExpr False)
  return $ foldAll body binds
  where
    binder  = pBinder True
    binders = do
     b  <- binder <* space
     bs <- bindAll b $ ((symbol "->" >> return []) <|> binders)
     return $ b ++ bs

pSlf :: (Ord e, Monad m) => Parser e m Term
pSlf = label "a self type: \"@x A\"" $ do
  name <- symbol "@" >> (pName True) <* space
  body <- bind [name] $ pExpr False
  return $ Slf name body

pNew :: (Ord e, Monad m) => Parser e m Term
pNew = label "datatype introduction: \"data x\"" $ do
  symbol "data "
  expr <- pExpr True
  return $ New expr

pUse :: (Ord e, Monad m) => Parser e m Term
pUse = label "a case expression: \"case x\"" $ do
  symbol "case "
  expr <- pExpr True
  return $ Use expr

pDecl :: (Ord e, Monad m) => Bool -> Parser e m (Name, Term, Term)
pDecl shadow = do
  nam    <- (pName True) <* space
  refs   <- asks _refs
  when (not shadow && M.member nam refs)
    (customFailure $ TopLevelRedefinition nam)
  bs      <- ((symbol ":" >> return []) <|> binders)
  let ns  = (fst3 <$> bs)
  typBody <- bind ns (pExpr False)
  let typ = foldAll typBody bs
  expBody <- symbol "=" >> bind (nam:ns) (pExpr False)
  let exp = foldLam expBody (fst3 <$> bs)
  return (nam, exp, typ)
  where
    binder  = pBinder False
    binders = do
     b  <- binder <* space
     bs <- bind (fst3 <$> b) $ ((symbol ":" >> return []) <|> binders)
     return $ b ++ bs

-- | Parse a local, possibly recursive, definition
pLet :: (Ord e, Monad m) => Parser e m Term
pLet = do
  rec <- (symbol "letrec" >> return True) <|> (symbol "let" >> return False)
  use   <- pUses
  (nam,exp,typ) <- pDecl True <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  return $ Let rec nam use typ exp bdy

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: (Ord e, Monad m) => Parser e m Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  env <- ask
  nam <- pName False
  case findByName nam (_context env) of
    Just i  -> return $ Var nam i
    Nothing -> case M.lookup nam (_refs env) of
      Just (def,trm) -> return (Ref nam def trm)
      Nothing        -> customFailure $ UndefinedReference nam

pAnn :: (Ord e, Monad m) => Parser e m Term
pAnn = do
  symbol "("
  val <- pExpr False <* space
  symbol "::"
  typ <- pExpr False <* space
  string ")"
  return $ Ann val typ
