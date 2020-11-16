module Yatima.Core.CheckError where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Yatima.Core.Ctx (Ctx (..), (<|))
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Core.Hoas
import Yatima.Term

data CheckError
  = CheckQuantityMismatch (Maybe Text) Loc Context (Name, Uses, Hoas) (Name, Uses, Hoas)
  | InferQuantityMismatch (Maybe Text) Loc Context (Name, Uses, Hoas) (Name, Uses, Hoas)
  | TypeMismatch (Maybe Text) Loc PreContext Hoas Hoas
  | UnboundVariable (Maybe Text) Loc Name Int
  | EmptyContext (Maybe Text) Loc
  | UntypedLambda (Maybe Text) Loc
  | UndefinedReference (Maybe Text) Loc Name
  | LambdaNonFunctionType (Maybe Text) Loc PreContext Hoas Hoas Hoas
  | NewNonSelfType (Maybe Text) Loc PreContext Hoas Hoas Hoas
  | NonFunctionApplication (Maybe Text) Loc Context Hoas Hoas Hoas
  | NonSelfUse (Maybe Text) Loc Context Hoas Hoas Hoas
  | UseOnNonInductiveType (Maybe Text) Loc Context Hoas LitType
  | CustomErr (Maybe Text) Loc PreContext Text

---- * Pretty Printing Errors
prettyUses :: Uses -> Text
prettyUses None = "0"
prettyUses Affi = "&"
prettyUses Once = "1"
prettyUses Many = "ω"

prettyCtx :: Context -> Text
prettyCtx (Ctx ctx) = foldr go "" ctx
  where
    go :: (Name, (Uses, Hoas)) -> Text -> Text
    go (n, (u, t)) txt = T.concat ["- ", prettyCtxElem (n, u, t), "\n", txt]

prettyCtxElem :: (Name, Uses, Hoas) -> Text
prettyCtxElem ("", u, t) = T.concat [prettyUses u, " _: ", printHoas t]
prettyCtxElem (n, u, t) = T.concat [prettyUses u, " ", n, ": ", printHoas t]

prettyPre :: PreContext -> Text
prettyPre (Ctx ctx) = foldr go "" ctx
  where
    go :: (Name, Hoas) -> Text -> Text
    go (n, t) txt = T.concat ["- ", prettyPreElem (n, t), "\n", txt]

prettyPreElem :: (Name, Hoas) -> Text
prettyPreElem ("", t) = T.concat [" _: ", printHoas t]
prettyPreElem (n, t) = T.concat [" ", n, ": ", printHoas t]

printRange :: Maybe Text -> Loc -> Text
printRange Nothing _ = "Can't print ranges when directly checking DAG object."
printRange _ NoLoc = "IMPLEMENTATION ERROR: No `Loc` on `printRange`"
printRange (Just txt) (Loc from@(Pos fl fc) upto@(Pos ul uc)) =
  let ls = T.lines txt
      fl' = fl - 1
      fc' = fc - 1
      ul' = ul - 1
      uc' = uc - 1
      range = drop fl' (take (ul' + 1) ls)
   in T.concat
        [ "Error occurred from ",
          T.pack (show from),
          " to ",
          T.pack (show upto),
          ".\n",
          T.replicate fc' " ",
          "\ESC[31m\STX⇃\ESC[m\STX\n",
          T.unlines range,
          T.replicate uc' " ",
          "\ESC[31m\STX↾\ESC[m\STX",
          "\n"
        ]

prettyError :: CheckError -> Text
prettyError e = case e of
  CheckQuantityMismatch f l ctx a b ->
    T.concat
      [ "Type checking quantity mismatch: \n",
        "- Expected: ",
        prettyCtxElem a,
        "\n",
        "- Detected: ",
        prettyCtxElem b,
        "\n",
        "With context:\n",
        prettyCtx ctx,
        "\n",
        printRange f l
      ]
  InferQuantityMismatch f l ctx a b ->
    T.concat
      [ "Type inference quantity mismatch: \n",
        "- Expected: ",
        prettyCtxElem a,
        "\n",
        "- Inferred: ",
        prettyCtxElem b,
        "\n",
        "With context:\n",
        prettyCtx ctx,
        "\n",
        printRange f l
      ]
  TypeMismatch f l ctx a b ->
    T.concat
      [ "Type Mismatch: \n",
        "- Expected: ",
        prettyPreElem ("", a),
        "\n",
        "- Detected: ",
        prettyPreElem ("", b),
        "\n",
        "With context:\n",
        prettyPre ctx,
        "\n",
        printRange f l
      ]
  LambdaNonFunctionType f l ctx trm typ typ' ->
    T.concat
      [ "The type of a lambda must be a forall: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas typ,
        "\n",
        "  Reduced type: ",
        printHoas typ',
        "\n",
        "With context:\n",
        prettyPre ctx,
        "\n",
        printRange f l
      ]
  NewNonSelfType f l ctx trm typ typ' ->
    T.concat
      [ "The type of data must be a self: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas typ,
        "\n",
        "  Reduced type: ",
        printHoas typ',
        "\n",
        "With context:\n",
        prettyPre ctx,
        "\n",
        printRange f l
      ]
  UnboundVariable f l nam idx ->
    T.concat
      ["Unbound free variable: ", T.pack $ show nam, " at level ", T.pack $ show idx]
  UntypedLambda f l -> "Can't infer the type of a lambda"
  NonFunctionApplication f l ctx trm typ typ' ->
    T.concat
      [ "Tried to apply something that wasn't a lambda: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas typ,
        "\n",
        "  Reduced type: ",
        printHoas typ',
        "\n",
        "With context:\n",
        prettyCtx ctx,
        "\n",
        printRange f l
      ]
  NonSelfUse f l ctx trm typ typ' ->
    T.concat
      [ "Tried to case on something that isn't data: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas typ,
        "\n",
        "  Reduced type: ",
        printHoas typ',
        "\n",
        "With context:\n",
        prettyCtx ctx,
        "\n",
        printRange f l
      ]
  EmptyContext f l -> "Empty Context" <> "\n" <> printRange f l
  UndefinedReference f l name ->
    T.concat
      [ "UndefinedReference error: \n",
        "Name: ",
        T.pack $ show name,
        "\n",
        printRange f l
      ]
  UseOnNonInductiveType f l ctx trm typ ->
    T.concat
      [ "Cannot case on non-inductive primitive type: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas $ LTyH NoLoc typ,
        "\n",
        "With context:\n",
        prettyCtx ctx,
        "\n",
        printRange f l
      ]
  CustomErr f l ctx txt ->
    T.concat
      [ "Custom Error:\n",
        txt,
        "\n",
        "With context:\n",
        prettyPre ctx,
        "\n",
        printRange f l
      ]

instance Show CheckError where
  show e = T.unpack $ prettyError e
