module Main where

import           Control.Monad.State.Strict
import           Language.Yatima.Parse
import           Language.Yatima.Defs

import           Yide

main :: IO ()
main = evalStateT yide (YideState emptyDefs)
