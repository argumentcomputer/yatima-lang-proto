{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Yatima.Parse.Integer where

import qualified Data.Char                  as Char

import           Data.Proxy
import           Data.List (foldl')

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

binary
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
binary = mkNum
  <$> takeWhile1P Nothing (\x -> (isBinDigit x) || (x == '_'))
  <?> "binary integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_'   = a
    step a c     = a * 2 + fromIntegral (Char.digitToInt c)
    isBinDigit x = x == '0' || x == '1'
{-# INLINEABLE binary #-}



decimal :: (MonadParsec e s m, Token s ~ Char, Integral a) => m a
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.

decimal_
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
decimal_ = mkNum <$> takeWhile1P (Just "digit")
              (\x -> (Char.isDigit x) || (x == '_'))
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_'   = a
    step a c = a * 10 + fromIntegral (Char.digitToInt c)
{-# INLINE decimal_ #-}

hexadecimal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
hexadecimal = mkNum
  <$> takeWhile1P Nothing (\x -> (Char.isHexDigit x) || (x == '_'))
  <?> "hexadecimal integer"
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a '_'   = a
    step a c = a * 16 + fromIntegral (Char.digitToInt c)
{-# INLINEABLE hexadecimal #-}

