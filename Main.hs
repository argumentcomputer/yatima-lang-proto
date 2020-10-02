{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map                   (Map)
import qualified Data.Map                   as M

import Language.Yatima
import qualified Data.Text as Text

main :: IO ()
main = do

  testSynth M.empty
    "λ A B f g x => g (f (g (f x)))"
    "∀ (A: *) (B: *) (f: ∀(x: A) -> B) (g: ∀(x: B) -> A) (x: A) -> A"

  testSynth M.empty
    "λ A x => x"
    "∀ (A: *) (x: ?a) -> A"
