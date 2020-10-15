module Main where

import           Control.Monad.State.Strict

import           Data.Map (empty)
import           Path.IO
import           Language.Yatima

import           Yide

main :: IO ()
main = do
  dir  <- getCurrentDir
  root <- findYatimaRoot dir
  evalStateT yide (emptyYideState root)
