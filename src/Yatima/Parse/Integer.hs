{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Yatima.Parse.Integer
-- Description : This module implements parsers for integer literals in different
-- bases optionally separated by '_' characters
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This module modifies work by [the Megaparsec contributors](https://github.com/mrkkrp/megaparsec/blob/master/Text/Megaparsec/Char/Lexer.hs)
-- which is licensed under 2-Clause-BSD terms included with this package in the
-- @licenses/1999_present_Megaparsec@ file.
module Yatima.Parse.Integer where

import qualified Data.Char as Char
import Data.List (foldl')
import Data.Proxy
import Text.Megaparsec hiding (State)

binary ::
  forall e s m a.
  (MonadParsec e s m, Token s ~ Char, Integral a) =>
  m a
binary =
  mkNum
    <$> takeWhile1P Nothing (\x -> (isBinDigit x) || (x == '_'))
    <?> "binary integer"
  where
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_' = a
    step a c = a * 2 + fromIntegral (Char.digitToInt c)
    isBinDigit x = x == '0' || x == '1'
{-# INLINEABLE binary #-}

decimal :: (MonadParsec e s m, Token s ~ Char, Integral a) => m a
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.
decimal_ ::
  forall e s m a.
  (MonadParsec e s m, Token s ~ Char, Integral a) =>
  m a
decimal_ =
  mkNum
    <$> takeWhile1P
      (Just "digit")
      (\x -> (Char.isDigit x) || (x == '_'))
  where
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_' = a
    step a c = a * 10 + fromIntegral (Char.digitToInt c)
{-# INLINE decimal_ #-}

hexadecimal ::
  forall e s m a.
  (MonadParsec e s m, Token s ~ Char, Integral a) =>
  m a
hexadecimal =
  mkNum
    <$> takeWhile1P Nothing (\x -> (Char.isHexDigit x) || (x == '_'))
    <?> "hexadecimal integer"
  where
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_' = a
    step a c = a * 16 + fromIntegral (Char.digitToInt c)
{-# INLINEABLE hexadecimal #-}
