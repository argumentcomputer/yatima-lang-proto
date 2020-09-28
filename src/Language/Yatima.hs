module Language.Yatima where

import Language.Yatima.Uses
import qualified Language.Yatima.HOAS as HOAS
import Language.Yatima.HOAS (HOAS, Error)
import qualified Language.Yatima.Print as Print
import qualified Language.Yatima.Parse as Parse
import qualified Language.Yatima.Term as Term
import Language.Yatima.Term (Term)
import qualified Data.Text as Text
import Data.Text (Text)

parseTerm :: Text -> Maybe Term
parseTerm = Parse.parseTerm

unsafeParseTerm :: Text -> Term
unsafeParseTerm = Parse.unsafeParseTerm

whnf :: Term -> Term
whnf = HOAS.hoasToTerm [] . HOAS.whnf . HOAS.termToHoas []

norm :: Term -> Term
norm = HOAS.hoasToTerm [] . HOAS.norm . HOAS.termToHoas []

infer :: Term -> Either Error Term
infer term =
  let hTerm = HOAS.termToHoas [] term in
  case HOAS.infer [] Once hTerm of
    Left err -> Left err
    Right (_,ty) -> Right (HOAS.hoasToTerm [] ty)

check :: Term -> Term -> Either Error Term
check term tipo =
  let hTerm = HOAS.termToHoas [] term in
  let hTipo = HOAS.termToHoas [] tipo in
  case HOAS.check [] Once hTerm hTipo of
    Left err     -> Left err
    Right (_,ty) -> Right (HOAS.hoasToTerm [] ty)

synth :: Term -> Term -> Either Error (Term, Term)
synth term tipo =
  let hTerm = HOAS.termToHoas [] term in
  let hTipo = HOAS.termToHoas [] tipo in
  case HOAS.synth hTerm hTipo of
    Left err -> Left err
    Right tt -> Right (HOAS.hoasToTerm [] (fst tt), HOAS.hoasToTerm [] (snd tt))

prettyTerm :: Term -> Text
prettyTerm = Print.prettyTerm
    
prettyInfer :: Term -> Text
prettyInfer term = case infer term of
  Left err -> HOAS.prettyError err
  Right ty -> Print.prettyTerm ty

prettyCheck :: Term -> Term -> Text
prettyCheck term tipo = case check term tipo of
  Left err -> HOAS.prettyError err
  Right ty -> Print.prettyTerm ty

prettySynth :: Term -> Term -> Text
prettySynth term tipo = case synth term tipo of
  Left err -> HOAS.prettyError err
  Right (ty,tr) -> Text.concat [Print.prettyTerm tr, " :: ", Print.prettyTerm ty]

testSynth :: Text -> Text -> IO ()
testSynth termCode typeCode = do
  let term = unsafeParseTerm termCode
  let tipo = unsafeParseTerm typeCode
  putStrLn ("input-term: " ++ Text.unpack (prettyTerm term))
  putStrLn ("input-type: " ++ Text.unpack (prettyTerm tipo))
  case synth term tipo of
    Left err -> print (Text.unpack (HOAS.prettyError err))
    Right (sTerm, sTipo) -> do
      putStrLn ("synth-term: " ++ Text.unpack (prettyTerm sTerm))
      putStrLn ("synth-type: " ++ Text.unpack (prettyTerm sTipo))
