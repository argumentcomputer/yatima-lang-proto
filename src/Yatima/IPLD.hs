{-
Module      : Yatima.IPLD
Description : This module implements the IPLD embedding for Yatima terms and
definitions, as well as related utilities.
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPLD where

import           Codec.Serialise
import           Control.Monad.Except
import           Control.Monad.Identity

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.IPLD.CID
import           Data.IPLD.DagAST
import           Data.Text              (Text)
import qualified Data.Text              as T hiding (find)

import           Yatima.Package
import           Yatima.Term

termToAST :: Term -> DagAST
termToAST t = go t
  where
    go :: Term -> DagAST
    go t = case t of
      Var n i         -> Ctor "Var" [Vari i]
      Ref n _ cid     -> Ctor "Ref" [Link cid]
      Lit x           -> Ctor "Lit" [Data (BSL.toStrict $ serialise x)]
      LTy x           -> Ctor "LTy" [Data (BSL.toStrict $ serialise x)]
      Opr x           -> Ctor "Opr" [Data (BSL.toStrict $ serialise x)]
      Lam n b         -> Ctor "Lam" [bind b]
      App f a         -> Ctor "App" [go f, go a]
      New e           -> Ctor "New" [go e]
      Use e           -> Ctor "Use" [go e]
      Ann v t         -> Ctor "Ann" [go v, go t]
      Let r n u t x b -> Ctor "Let" [rec r, use u, go t, bind x, bind b]
      Typ             -> Ctor "Typ" []
      All n u t b     -> Ctor "All" [use u, go t, bind b]
      Slf n b         -> Ctor "Slf" [bind b]
    rec r = Data (BSL.toStrict $ serialise r)
    use u = Data (BSL.toStrict $ serialise u)
    bind t = Bind (go t)

termToMeta :: Term -> DagMeta
termToMeta t = go t
  where
    go :: Term -> DagMeta
    go t = case t of
      Ref n cid _     -> MCtor [MLink n cid]
      Lam n b         -> MCtor [bind n b]
      App f a         -> MCtor [go f, go a]
      New e           -> MCtor [go e]
      Use e           -> MCtor [go e]
      Ann v t         -> MCtor [go v, go t]
      Let r n u t x b -> MCtor [leaf, leaf, go t, bind n x, bind n b]
      All n u t b     -> MCtor [leaf, go t, bind n b]
      Slf n b         -> MCtor [bind n b]
      Typ             -> MCtor []
      _               -> MCtor [leaf]
    bind n t = MBind n (go t)
    leaf = MLeaf

defToDag :: Def -> (DagAST,DagMeta,DagAST,DagMeta)
defToDag (Def _ term typ_) = runIdentity $ do
  let (termAST,termMeta)  = (termToAST term,termToMeta term)
  let (typeAST,typeMeta)  = (termToAST typ_,termToMeta typ_)
  return (termAST,termMeta,typeAST,typeMeta)

defToDagDef :: Def -> DagDef
defToDagDef def@(Def doc term typ_) = runIdentity $ do
  let (termAST,termMeta,typeAST,typeMeta) = defToDag def
  let termASTCid = makeCid termAST :: CID
  let typeASTCid = makeCid typeAST :: CID
  return $ DagDef termASTCid typeASTCid doc termMeta typeMeta

data DagError
  = FreeVariable [Name] DagAST DagMeta Int
  | NoDeserial [Name] DagAST DagMeta DeserialiseFailure
  | LetNameMismatch [Name] DagAST DagMeta Name
  | UnexpectedASTMeta [Name] DagAST DagMeta DagAST DagMeta
  deriving Eq

deriving instance Ord DeserialiseFailure
deriving instance Ord DagError

dagToTerm :: [Name] -> DagAST -> DagMeta -> Except DagError Term
dagToTerm ns ast meta = case (ast,meta) of
  (Ctor "Ref" [Link t], MCtor [MLink n d]) -> return $ Ref n d t
  (Ctor "Lit" [Data bs], MCtor [MLeaf])    -> Lit <$> deserial @Literal bs
  (Ctor "LTy" [Data bs], MCtor [MLeaf])    -> LTy <$> deserial @LitType bs
  (Ctor "Opr" [Data bs], MCtor [MLeaf])    -> Opr <$> deserial @PrimOp bs
  (Ctor "Typ" [], MCtor [])                -> return Typ
  (Ctor "App" [f,a], MCtor [fm,am])        -> App <$> go ns f fm <*> go ns a am
  (Ctor "Ann" [v,t], MCtor [vm,tm])        -> Ann <$> go ns v vm <*> go ns t tm
  (Ctor "New" [e], MCtor [em])             -> New <$> go ns e em
  (Ctor "Use" [e], MCtor [em])             -> Use <$> go ns e em
  (Ctor "Lam" [Bind b], MCtor [MBind n m]) -> Lam n <$> go (n:ns) b m
  (Ctor "Slf" [Bind b], MCtor [MBind n m]) -> Slf n <$> go (n:ns) b m
  (Ctor "All" [Data u, t, Bind b], MCtor [MLeaf, tm, MBind n bm]) -> do
    All n <$> deserial @Uses u <*> go ns t tm <*> go (n:ns) b bm
  (Ctor "Let" [Data r, Data u, t, Bind x, Bind b],
    MCtor [MLeaf, MLeaf, tm, MBind n xm, MBind n' bm]) -> do
    when (n /= n') (throwError $ LetNameMismatch ns ast meta n)
    Let <$> deserial @Bool r <*> pure n <*> deserial @Uses u
        <*> go ns t tm <*> go (n:ns) x xm <*> go (n:ns) b bm
  (Ctor "Var" [Vari i], MCtor [MLeaf]) ->
    case findByInt i ns of
      Just n  -> return $ Var n i
      Nothing -> throwError $ FreeVariable ns ast meta i
  (ast', meta') -> throwError $ UnexpectedASTMeta ns ast meta ast' meta'
  where
    go = dagToTerm
    deserial :: Serialise a => BS.ByteString -> Except DagError a
    deserial bs = do
      let err = (NoDeserial ns ast meta)
      withExceptT err (liftEither $ deserialiseOrFail (BSL.fromStrict bs))

dagToDef :: Text -> Name -> (DagAST,DagMeta) -> (DagAST,DagMeta)
         -> Except DagError Def
dagToDef doc n (termAST, termMeta) (typeAST,typeMeta) = do
  trm <- dagToTerm [n] termAST termMeta
  typ <- dagToTerm [] typeAST typeMeta
  return $ Def doc trm typ

instance Show DagError where
  show e = case e of
    FreeVariable ctx ast meta idx -> concat
      ["DagError: Free Variable ", show idx
      , "\ncontext: ", show ctx
      , "\nDagAST: ", show ast
      , "\nDagMeta: ", show meta
      ]
    NoDeserial ctx ast meta err -> concat
      ["DagError: Deserialisation failure ", show err
      , "\ncontext: ", show ctx
      , "\nDagAST: ", show ast
      , "\nDagMeta: ", show meta
      ]
    LetNameMismatch ctx ast meta n -> concat
      ["DagError: Name mismatch in Let constructor ", show n
      , "\ncontext: ", show ctx
      , "\nDagAST: ", show ast
      , "\nDagMeta: ", show meta
      ]
    UnexpectedASTMeta ctx ast meta ast' meta' -> concat
      ["DagError: Unexpected DagAST and DagMeta"
      , "\nunexpected DagAST: ", show ast'
      , "\nunexpected DagMeta: ", show meta'
      , "\ncontext: ", show ctx
      , "\nentire DagAST: ", show ast
      , "\nentire DagMeta: ", show meta
      ]




