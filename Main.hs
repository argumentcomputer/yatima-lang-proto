{-# LANGUAGE OverloadedStrings #-}

import Language.Yatima
import qualified Data.Text as Text

main :: IO ()
main = do

  testSynth
    "λ A B f g x => g (f (g (f x)))"
    "∀ (A: *) (B: *) (f: ∀(x: A) -> B) (g: ∀(x: B) -> A) (x: A) -> A"

  testSynth
    "λ A x => x"
    "∀ (A: *) (x: ?a) -> A"
