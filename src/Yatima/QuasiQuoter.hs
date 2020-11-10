{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Yatima.QuasiQuoter
-- Description : A quasiquoter to allow Yatima expressions to be embedded in
-- Haskell source files.
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.QuasiQuoter where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Generics.Aliases
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Language.Haskell.TH hiding (Name)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Name)
import qualified Language.Haskell.TH.Syntax as TH
import Text.Megaparsec
import Yatima.Parse
import Yatima.Parse.Parser
import Yatima.Parse.Term
import Yatima.Term

yatima :: QuasiQuoter
yatima =
  QuasiQuoter
    { quoteExp = yatima',
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

liftText :: T.Text -> Maybe ExpQ
liftText txt = Just $ appE (varE 'T.pack) $ litE $ StringL (T.unpack txt)

liftByteString :: ByteString -> Maybe ExpQ
liftByteString txt = Just $ appE (varE 'B8.pack) $ litE $ StringL (B8.unpack txt)

yatima' :: String -> Q Exp
yatima' s = do
  file <- loc_filename <$> location
  thExts <- extsEnabled
  let wrapFromString e =
        if OverloadedStrings `elem` thExts
          then [|fromString $(e)|]
          else e
  let env = defaultParseEnv
  let p = (space >> pExpr True)
  case runIdentity (parseM @() @Identity p env file (T.pack s)) of
    Left err -> do
      err' <- overrideErrorForFile file err
      fail (errorBundlePretty err')
    Right c -> dataToExpQ (const Nothing `extQ` liftText `extQ` liftByteString) c

overrideErrorForFile :: FilePath -> ParseErrorBundle Text e -> Q (ParseErrorBundle Text e)
overrideErrorForFile "<interactive>" err = pure err
overrideErrorForFile filename err = do
  (lin, col) <- loc_start <$> location
  fileContent <- runIO (readFile filename)
  let (prefix, postfix) = splitAt (col - 1) $ unlines $ drop (lin - 1) (lines fileContent)
  pure $
    err
      { bundlePosState =
          (bundlePosState err)
            { pstateInput = T.pack postfix,
              pstateSourcePos = SourcePos filename (mkPos lin) (mkPos col),
              pstateOffset = 0,
              pstateLinePrefix = prefix
            }
      }
