{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Yatima.QuasiQuoter where

import           Control.Monad.Identity

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Data.Typeable

import           Text.Megaparsec

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Yatima.Term
import           Yatima.Parse

yatima :: QuasiQuoter
yatima = QuasiQuoter
  { quoteExp  = toExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

toExp :: String -> Q Exp
toExp s = do
  file <- loc_filename <$> location
  thExts <- extsEnabled
  let wrapFromString e =
        if OverloadedStrings `elem` thExts
          then [|fromString $(e)|]
          else e
  let env = defaultParseEnv
  let p   = (space >> pExpr True)
  case runIdentity (parseM @() @Identity p env file (T.pack s)) of
    Left err -> do
      err' <- overrideErrorForFile file err
      fail (errorBundlePretty err')
    Right c -> dataToExpQ (\a -> liftText <$> cast a) c

overrideErrorForFile :: FilePath -> ParseErrorBundle Text e -> Q (ParseErrorBundle Text e)
overrideErrorForFile "<interactive>" err = pure err
overrideErrorForFile filename err = do
  (line, col) <- loc_start <$> location
  fileContent <- runIO (readFile filename)
  let (prefix, postfix) = splitAt (col - 1) $ unlines $ drop (line - 1) (lines fileContent)
  pure $
    err
      { bundlePosState =
          (bundlePosState err)
            { pstateInput = T.pack postfix,
              pstateSourcePos = SourcePos filename (mkPos line) (mkPos col),
              pstateOffset = 0,
              pstateLinePrefix = prefix
            }
      }
