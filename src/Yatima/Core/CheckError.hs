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
  = CheckQuantityMismatch Context (Name, Uses, Hoas) (Name, Uses, Hoas)
  | InferQuantityMismatch Context (Name, Uses, Hoas) (Name, Uses, Hoas)
  | TypeMismatch PreContext Hoas Hoas
  | UnboundVariable Name Int
  | EmptyContext
  | UntypedLambda
  | UndefinedReference Name
  | LambdaNonFunctionType PreContext Hoas Hoas Hoas
  | NewNonSelfType PreContext Hoas Hoas Hoas
  | NonFunctionApplication Context Hoas Hoas Hoas
  | NonSelfUse Context Hoas Hoas Hoas
  | UseOnNonInductiveType Context Hoas LitType
  | CustomErr PreContext Text

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

prettyError :: CheckError -> Text
prettyError e = case e of
  CheckQuantityMismatch ctx a b ->
    T.concat
      [ "Type checking quantity mismatch: \n",
        "- Expected: ",
        prettyCtxElem a,
        "\n",
        "- Detected: ",
        prettyCtxElem b,
        "\n",
        "With context:\n",
        prettyCtx ctx
      ]
  InferQuantityMismatch ctx a b ->
    T.concat
      [ "Type inference quantity mismatch: \n",
        "- Expected: ",
        prettyCtxElem a,
        "\n",
        "- Inferred: ",
        prettyCtxElem b,
        "\n",
        "With context:\n",
        prettyCtx ctx
      ]
  TypeMismatch ctx a b ->
    T.concat
      [ "Type Mismatch: \n",
        "- Expected: ",
        prettyPreElem ("", a),
        "\n",
        "- Detected: ",
        prettyPreElem ("", b),
        "\n",
        "With context:\n",
        prettyPre ctx
      ]
  LambdaNonFunctionType ctx trm typ typ' ->
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
        prettyPre ctx
      ]
  NewNonSelfType ctx trm typ typ' ->
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
        prettyPre ctx
      ]
  UnboundVariable nam idx ->
    T.concat
      ["Unbound free variable: ", T.pack $ show nam, " at level ", T.pack $ show idx]
  UntypedLambda -> "Can't infer the type of a lambda"
  NonFunctionApplication ctx trm typ typ' ->
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
        prettyCtx ctx
      ]
  NonSelfUse ctx trm typ typ' ->
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
        prettyCtx ctx
      ]
  EmptyContext -> "Empty Context"
  UndefinedReference name ->
    T.concat
      [ "UndefinedReference error: \n",
        "Name: ",
        T.pack $ show name,
        "\n"
      ]
  UseOnNonInductiveType ctx trm typ ->
    T.concat
      [ "Cannot case on non-inductive primitive type: \n",
        "  Checked term: ",
        printHoas trm,
        "\n",
        "  Against type: ",
        printHoas $ LTyH typ,
        "\n",
        "With context:\n",
        prettyCtx ctx
      ]
  CustomErr ctx txt ->
    T.concat
      [ "Custom Error:\n",
        txt,
        "\n",
        "With context:\n",
        prettyPre ctx
      ]

instance Show CheckError where
  show e = T.unpack $ prettyError e
