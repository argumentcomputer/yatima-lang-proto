module Main where

import           Control.Monad.State.Strict

import           Data.Map (empty)

import           Yide

main :: IO ()
main = evalStateT yide (emptyYideState "/home/john/Documents/yatima/test")
--main = evalStateT yide (YideState empty)
