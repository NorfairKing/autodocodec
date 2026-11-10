{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Nix.Options
  ( -- * Producing a Nixos module type
    renderNixOptionTypeViaCodec,
    renderNixOptionsViaCodec,
    renderNixOptionTypeVia,
    renderNixOptionsVia,
    valueCodecNixOptionType,
    objectCodecNixOptions,
    Option (..),
    emptyOption,
    simplifyOption,
    OptionType (..),
    simplifyOptionType,
    renderOption,
    renderOptionType,
    withNixArgs,
    optionExpression,
    optionExpr,
    optionsExpression,
    optionsExpr,
    optionTypeExpression,
    optionTypeExpr,
    renderExpression,
  )
where

import Autodocodec
import Autodocodec.Nix.Expression
import Autodocodec.Nix.Render
import Control.Applicative
import Data.Aeson as JSON
import Data.Containers.ListUtils
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

renderNixOptionTypeViaCodec :: forall a. (HasCodec a) => Text
renderNixOptionTypeViaCodec = renderNixOptionTypeVia (codec @a)

renderNixOptionsViaCodec :: forall a. (HasObjectCodec a) => Text
renderNixOptionsViaCodec = renderNixOptionsVia (objectCodec @a)

renderNixOptionTypeVia :: ValueCodec input output -> Text
renderNixOptionTypeVia =
  renderOptionType
    . fromMaybe (OptionTypeSimple "lib.types.anything")
    . valueCodecNixOptionType

renderNixOptionsVia :: ObjectCodec input output -> Text
renderNixOptionsVia =
  renderOptions
    . objectCodecNixOptions

valueCodecNixOptionType :: ValueCodec input output -> Maybe OptionType
valueCodecNixOptionType = fmap simplifyOptionType . go
  where
    mTyp = fromMaybe $ OptionTypeSimple "lib.types.anything"
    go :: ValueCodec input output -> Maybe OptionType
    go = \case
      NullCodec -> Just OptionTypeNull
      BoolCodec _ -> Just $ OptionTypeSimple "lib.types.bool"
      StringCodec _ _ -> Just $ OptionTypeSimple "lib.types.str"
      IntegerCodec _ bounds -> Just $
        OptionTypeSimple $
          case guessIntegerBoundsSymbolic bounds of
            BitUInt w -> case w of
              64 -> "lib.types.ints.unsigned"
              32 -> "lib.types.ints.u32"
              16 -> "lib.types.ints.u16"
              8 -> "lib.types.ints.u8"
              _ -> "lib.types.int" -- TODO bounds?
            BitSInt w -> case w of
              64 -> "lib.types.int"
              32 -> "lib.types.ints.s32"
              16 -> "lib.types.ints.s16"
              8 -> "lib.types.ints.s8"
              _ -> "lib.types.int" -- TODO bounds?
            OtherIntegerBounds _ _ -> "lib.types.int" -- TODO bounds?
      NumberCodec _ _ -> Just $ OptionTypeSimple "lib.types.number"
      HashMapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      MapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      ValueCodec -> Just (OptionTypeSimple "lib.types.unspecified")
      ArrayOfCodec _ c -> Just $ OptionTypeListOf $ mTyp $ go c
      ObjectOfCodec _ oc -> Just $ OptionTypeSubmodule $ objectCodecNixOptions oc
      EqCodec v c -> Just $ OptionTypeEnum [toNixExpressionVia c v]
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> Just $ OptionTypeOneOf (map mTyp [go c1, go c2])
      CommentCodec _ c -> go c
      ReferenceCodec {} -> Nothing -- TODO: let-binding?

-- | A key that may be left out gets an option without a default, so that
-- leaving it out and setting it to 'null' can be told apart.
-- Reading such an option throws, so every generated submodule has a read-only
-- @__defined@ option instead: what was defined in it, and nothing that was not.
objectCodecNixOptions :: ObjectCodec input output -> Map Text Option
objectCodecNixOptions = simplifyOptions . go
  where
    go :: ObjectCodec input output -> Map Text Option
    go = \case
      DiscriminatedUnionCodec k _ m ->
        M.insert
          k
          ( Option
              { optionType = Just $ OptionTypeOneOf $ map (OptionTypeSimple . T.pack . show) $ HM.keys m,
                optionDescription = Nothing,
                optionDefault = Nothing
              }
          )
          $ M.unionsWith
            ( \t1 t2 ->
                Option
                  { optionType = Just $ OptionTypeOneOf $ map (fromMaybe (OptionTypeSimple "lib.types.anything") . optionType) [t1, t2],
                    optionDescription = optionDescription t1 <|> optionDescription t2, -- TODO
                    optionDefault = Nothing
                  }
            )
          $ map (go . snd)
          $ HM.elems m
      RequiredKeyCodec key o mDesc -> M.singleton key $ undefaultedOption o mDesc
      OptionalKeyCodec key o mDesc -> M.singleton key $ undefaultedOption o mDesc
      OptionalKeyWithDefaultCodec key c defaultValue mDesc ->
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType c,
              optionDescription = optionDescriptionFor mDesc c,
              optionDefault = Just $ toJSONVia c defaultValue
            }
      OptionalKeyWithOmittedDefaultCodec key c defaultValue mDesc ->
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType c,
              optionDescription = optionDescriptionFor mDesc c,
              optionDefault = Just $ toJSONVia c defaultValue
            }
      PureCodec _ -> M.empty
      ApCodec c1 c2 -> M.unionWith mergeOption (go c1) (go c2)
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> M.unionWith mergeOption (go c1) (go c2)
    undefaultedOption :: ValueCodec input output -> Maybe Text -> Option
    undefaultedOption c mDesc =
      Option
        { optionType = valueCodecNixOptionType c,
          optionDescription = optionDescriptionFor mDesc c,
          optionDefault = Nothing
        }
    -- This throwing away of the description and the default is not ideal but
    -- better than just taking the first option.
    mergeOption :: Option -> Option -> Option
    mergeOption o1 o2 =
      o1
        { optionType =
            ( \ot1 ot2 ->
                simplifyOptionType $
                  OptionTypeOneOf
                    [ ot1,
                      ot2
                    ]
            )
              <$> optionType o1
              <*> optionType o2
        }

-- | What an option says about itself: the field's own description, and then
-- whatever comments the codec under it carries.
--
-- Both, because the two say the same kind of thing in two places. A field's
-- description is written where the field is and a comment is written where
-- the type is, and whoever reads the generated option wants what each of them
-- knows. Keeping only the description is how documentation written with
-- '<?>' silently fails to reach a NixOS module.
optionDescriptionFor :: Maybe Text -> ValueCodec input output -> Maybe Text
optionDescriptionFor mDesc c =
  case filter (not . T.null) $ map T.strip $ maybeToList mDesc ++ valueCodecComments c of
    [] -> Nothing
    ds -> Just $ T.intercalate "\n" ds

-- | The comments on a value codec, outermost first.
--
-- Through a 'BimapCodec' as 'valueCodecNixOptionType' is, because a codec
-- written for one type in terms of another is still that type's codec, and a
-- comment on the one underneath documents it.
--
-- Through the 'EitherCodec' that 'maybeCodec' is, for the same reason: a
-- codec made nullable is still that codec, and 'optionalFieldOrNullWith'
-- builds its field that way, so a comment there would otherwise be the one
-- kind that silently went missing.
--
-- Not into any other 'EitherCodec', nor into an 'ArrayOfCodec', whose
-- comments are about something the option is made of rather than about the
-- option.
valueCodecComments :: ValueCodec input output -> [Text]
valueCodecComments = \case
  CommentCodec comment c -> comment : valueCodecComments c
  BimapCodec _ _ c -> valueCodecComments c
  EitherCodec _ NullCodec c -> valueCodecComments c
  EitherCodec _ c NullCodec -> valueCodecComments c
  _ -> []

data Option = Option
  { optionType :: !(Maybe OptionType),
    optionDescription :: !(Maybe Text),
    optionDefault :: !(Maybe JSON.Value)
  }
  deriving (Show, Eq, Ord)

emptyOption :: Option
emptyOption =
  Option
    { optionType = Nothing,
      optionDescription = Nothing,
      optionDefault = Nothing
    }

simplifyOption :: Option -> Option
simplifyOption o = o {optionType = simplifyOptionType <$> optionType o}

data OptionType
  = OptionTypeNull
  | OptionTypeSimple !Text
  | OptionTypeEnum ![Expression]
  | OptionTypeNullOr !OptionType
  | OptionTypeListOf !OptionType
  | OptionTypeAttrsOf !OptionType
  | OptionTypeOneOf ![OptionType]
  | OptionTypeSubmodule !(Map Text Option)
  deriving (Show, Eq, Ord)

simplifyOptionType :: OptionType -> OptionType
simplifyOptionType = go
  where
    go = \case
      OptionTypeNull -> OptionTypeNull
      OptionTypeSimple t -> OptionTypeSimple t
      OptionTypeEnum es -> OptionTypeEnum es
      OptionTypeNullOr t -> case t of
        OptionTypeNull -> OptionTypeNull
        OptionTypeNullOr t' -> go $ OptionTypeNullOr t'
        OptionTypeOneOf os -> OptionTypeNullOr $ go $ OptionTypeOneOf $ filter (/= OptionTypeNull) $ map go os
        _ -> OptionTypeNullOr $ go t
      OptionTypeListOf o -> OptionTypeListOf $ go o
      OptionTypeAttrsOf o -> OptionTypeAttrsOf $ go o
      OptionTypeOneOf os -> case goEnums $ nubOrd $ concatMap goOr os of
        [ot] -> ot
        os' ->
          if any canBeNull os'
            then go $ OptionTypeNullOr $ case mapMaybe stripNull os' of
              [t] -> t
              ts' -> OptionTypeOneOf ts'
            else OptionTypeOneOf os'
      OptionTypeSubmodule m -> OptionTypeSubmodule $ M.map goOpt m

    canBeNull :: OptionType -> Bool
    canBeNull = \case
      OptionTypeNull -> True
      OptionTypeNullOr _ -> True
      _ -> False

    stripNull :: OptionType -> Maybe OptionType
    stripNull = \case
      OptionTypeNull -> Nothing
      OptionTypeNullOr t -> Just t
      t -> Just t

    goEnums :: [OptionType] -> [OptionType]
    goEnums = goEnum []
      where
        goEnum :: [Expression] -> [OptionType] -> [OptionType]
        goEnum es = \case
          [] -> case es of
            [] -> []
            _ -> [OptionTypeEnum es]
          (t : rest) -> case t of
            OptionTypeEnum es' -> goEnum (es ++ es') rest
            _ -> t : goEnum es rest

    goOpt o = o {optionType = go <$> optionType o}

    goOr = \case
      OptionTypeOneOf os -> concatMap goOr os
      o -> [o]

simplifyOptions :: Map Text Option -> Map Text Option
simplifyOptions = M.map simplifyOption

renderOption :: Option -> Text
renderOption = renderExpression . withNixArgs . optionExpression

renderOptions :: Map Text Option -> Text
renderOptions = renderExpression . withNixArgs . optionsExpression

renderOptionType :: OptionType -> Text
renderOptionType = renderExpression . withNixArgs . optionTypeExpression

withNixArgs :: Expression -> Expression
withNixArgs = ExprFun ["lib"]

-- {-# DEPRECATED optionExpr "Use optionExpression instead" #-}
optionExpr :: Option -> Expression
optionExpr = optionExpression

optionExpression :: Option -> Expression
optionExpression Option {..} =
  ExprAp
    (ExprVar "lib.mkOption")
    ( ExprAttrSet $
        M.fromList $
          concat
            [ [("type", optionTypeExpression typ) | typ <- maybeToList optionType],
              [("description", ExprLitString d) | d <- maybeToList optionDescription],
              case optionDefault of
                Nothing -> []
                Just d -> [("default", toNixExpressionViaCodec d)]
            ]
    )

-- {-# DEPRECATED optionsExpr "Use optionsExpression instead" #-}
optionsExpr :: Map Text Option -> Expression
optionsExpr = optionsExpression

optionsExpression :: Map Text Option -> Expression
optionsExpression = ExprAttrSet . M.map optionExpression

-- {-# DEPRECATED optionTypeExpr "Use optionTypeExpression instead" #-}
optionTypeExpr :: OptionType -> Expression
optionTypeExpr = optionTypeExpression

optionTypeExpression :: OptionType -> Expression
optionTypeExpression = go
  where
    go = \case
      OptionTypeNull -> ExprAp (ExprVar "lib.types.enum") (ExprLitList [ExprNull])
      OptionTypeSimple s -> ExprVar s
      OptionTypeEnum es -> ExprAp (ExprVar "lib.types.enum") (ExprLitList es)
      OptionTypeNullOr ot -> ExprAp (ExprVar "lib.types.nullOr") (go ot)
      OptionTypeListOf ot ->
        ExprAp
          (ExprVar "lib.types.listOf")
          (go ot)
      OptionTypeAttrsOf ot ->
        ExprAp
          (ExprVar "lib.types.attrsOf")
          (go ot)
      OptionTypeOneOf os ->
        ExprAp
          (ExprVar "lib.types.oneOf")
          (ExprLitList (map go os))
      OptionTypeSubmodule os ->
        ExprAp
          (ExprVar "lib.types.submodule")
          ( ExprFun ["config", "options", "..."] $
              ExprAttrSet $
                M.singleton "options" $
                  ExprAttrSet $
                    M.insert "__defined" (definedOptionExpression os) $
                      M.map optionExpression os
          )

definedOptionExpression :: Map Text Option -> Expression
definedOptionExpression os =
  ExprAp (ExprVar "lib.mkOption") $
    ExprAttrSet $
      M.fromList
        [ ("internal", ExprLitBool True),
          ("readOnly", ExprLitBool True),
          ( "default",
            ExprAp
              (ExprAp (ExprVar "lib.filterAttrs") (ExprVar "(name: _: options.${name}.isDefined)"))
              (ExprAttrSet (M.mapWithKey definedField os))
          )
        ]
  where
    definedField :: Text -> Option -> Expression
    definedField key o =
      let value = concat ["config.", T.unpack (nixAttrKey key)]
       in ExprVar $ T.pack $ fromMaybe value $ optionType o >>= definedValue value

-- | The defined part of a value of the given type, or 'Nothing' if that is
-- the whole value.
--
-- Of a @oneOf@, the first alternative that has the shape of the value is
-- looked into, as that is the one the module system merged it with.
definedValue :: String -> OptionType -> Maybe String
definedValue value = \case
  OptionTypeSubmodule _ -> Just $ concat [value, ".__defined"]
  OptionTypeNullOr t -> (\d -> unwords ["if", value, "== null then null else", d]) <$> definedValue value t
  OptionTypeListOf t -> (\d -> unwords ["map (v:", d ++ ")", value]) <$> definedValue "v" t
  OptionTypeAttrsOf t -> (\d -> unwords ["lib.mapAttrs (_: v:", d ++ ")", value]) <$> definedValue "v" t
  OptionTypeOneOf ts ->
    let alternative :: OptionType -> Maybe (String, String)
        alternative t = (,) <$> shapeCheck value t <*> definedValue value t
     in case concatMap (mapMaybe alternative . oneOfAlternatives) ts of
          [] -> Nothing
          as -> Just $ foldr (\(check, d) rest -> unwords ["if", check, "then", d, "else", rest]) value as
  OptionTypeNull -> Nothing
  OptionTypeSimple _ -> Nothing
  OptionTypeEnum _ -> Nothing

-- | The alternatives of a @oneOf@ that a value can have, with nested ones
-- flattened.
--
-- A null has no alternative of its own, so it is left as it is.
oneOfAlternatives :: OptionType -> [OptionType]
oneOfAlternatives = \case
  OptionTypeOneOf ts -> concatMap oneOfAlternatives ts
  OptionTypeNullOr t -> oneOfAlternatives t
  OptionTypeNull -> []
  t@(OptionTypeSimple _) -> [t]
  t@(OptionTypeEnum _) -> [t]
  t@(OptionTypeListOf _) -> [t]
  t@(OptionTypeAttrsOf _) -> [t]
  t@(OptionTypeSubmodule _) -> [t]

-- | Whether a value has the shape of the given type, for the types that
-- 'definedValue' looks into.
shapeCheck :: String -> OptionType -> Maybe String
shapeCheck value = \case
  OptionTypeSubmodule _ -> Just $ unwords [value, "? __defined"]
  OptionTypeListOf _ -> Just $ unwords ["builtins.isList", value]
  OptionTypeAttrsOf _ -> Just $ unwords ["builtins.isAttrs", value]
  OptionTypeNullOr _ -> Nothing
  OptionTypeOneOf _ -> Nothing
  OptionTypeNull -> Nothing
  OptionTypeSimple _ -> Nothing
  OptionTypeEnum _ -> Nothing
