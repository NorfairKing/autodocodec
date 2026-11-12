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
    renderNixOptionTypeVia,
    valueCodecNixOptionType,
    objectCodecNixOptions,
    Option (..),
    OptionRequirement (..),
    emptyOption,
    simplifyOption,
    OptionType (..),
    simplifyOptionType,
    renderOption,
    renderOptionType,
    withNixArgs,
    optionExpression,
    optionExpr,
    optionTypeExpression,
    optionTypeExpr,
    renderExpression,
  )
where

import Autodocodec
import Autodocodec.Nix.Expression
import Autodocodec.Nix.Render
import Data.Aeson as JSON
import Data.Containers.ListUtils
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Merge.Strict as Merge
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

renderNixOptionTypeViaCodec :: forall a. (HasCodec a) => Text
renderNixOptionTypeViaCodec = renderNixOptionTypeVia (codec @a)

renderNixOptionTypeVia :: ValueCodec input output -> Text
renderNixOptionTypeVia =
  renderOptionType
    . fromMaybe (OptionTypeSimple "lib.types.anything")
    . valueCodecNixOptionType

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
-- The value of a generated submodule is what was defined in it, and nothing
-- that was not.
-- Reading it throws when a required option was not defined.
objectCodecNixOptions :: ObjectCodec input output -> Map Text Option
objectCodecNixOptions = simplifyOptions . go
  where
    go :: ObjectCodec input output -> Map Text Option
    go = \case
      DiscriminatedUnionCodec k _ m ->
        M.insert
          k
          ( Option
              { optionType = Just $ OptionTypeEnum $ map ExprLitString $ HM.keys m,
                optionDescription = Nothing,
                optionDefault = Nothing,
                optionRequirement = OptionRequired
              }
          )
          $ case map (go . snd) $ HM.elems m of
            [] -> M.empty
            alternative : alternatives -> foldr mergeAlternatives alternative alternatives
      RequiredKeyCodec key o mDesc -> M.singleton key $ undefaultedOption OptionRequired o mDesc
      OptionalKeyCodec key o mDesc -> M.singleton key $ undefaultedOption OptionOptional o mDesc
      OptionalKeyWithDefaultCodec key c defaultValue mDesc ->
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType c,
              optionDescription = optionDescriptionFor mDesc c,
              optionDefault = Just $ toJSONVia c defaultValue,
              optionRequirement = OptionOptional
            }
      OptionalKeyWithOmittedDefaultCodec key c defaultValue mDesc ->
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType c,
              optionDescription = optionDescriptionFor mDesc c,
              optionDefault = Just $ toJSONVia c defaultValue,
              optionRequirement = OptionOptional
            }
      PureCodec _ -> M.empty
      ApCodec c1 c2 ->
        M.unionWith
          ( \o1 o2 ->
              (mergeOption o1 o2)
                { optionRequirement = requiredByEither (optionRequirement o1) (optionRequirement o2)
                }
          )
          (go c1)
          (go c2)
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> mergeAlternatives (go c1) (go c2)
    undefaultedOption :: OptionRequirement -> ValueCodec input output -> Maybe Text -> Option
    undefaultedOption requirement c mDesc =
      Option
        { optionType = valueCodecNixOptionType c,
          optionDescription = optionDescriptionFor mDesc c,
          optionDefault = Nothing,
          optionRequirement = requirement
        }
    requiredByEither :: OptionRequirement -> OptionRequirement -> OptionRequirement
    requiredByEither r1 r2 = case r1 of
      OptionRequired -> OptionRequired
      OptionOptional -> r2

-- | The options of a choice between objects.
--
-- A value for any one alternative is a valid value, so a key is only required
-- when every alternative requires it, and only keeps a default when every
-- alternative has the same one.
mergeAlternatives :: Map Text Option -> Map Text Option -> Map Text Option
mergeAlternatives =
  let onlyInOne :: Option -> Option
      onlyInOne o =
        o
          { optionRequirement = OptionOptional,
            optionDefault = Nothing
          }
      requiredByBoth :: OptionRequirement -> OptionRequirement -> OptionRequirement
      requiredByBoth r1 r2 = case r1 of
        OptionRequired -> r2
        OptionOptional -> OptionOptional
   in Merge.merge
        (Merge.mapMissing (const onlyInOne))
        (Merge.mapMissing (const onlyInOne))
        ( Merge.zipWithMatched
            ( \_ o1 o2 ->
                (mergeOption o1 o2)
                  { optionRequirement = requiredByBoth (optionRequirement o1) (optionRequirement o2)
                  }
            )
        )

-- | Two options for the same key.
--
-- Keeping only the first description is not ideal but better than dropping
-- both.
-- A default is only kept when both agree on it, because the value of the
-- option is written out whichever alternative the rest of the value is for.
mergeOption :: Option -> Option -> Option
mergeOption o1 o2 =
  o1
    { optionDefault =
        if optionDefault o1 == optionDefault o2
          then optionDefault o1
          else Nothing,
      optionType =
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
    optionDefault :: !(Maybe JSON.Value),
    optionRequirement :: !OptionRequirement
  }
  deriving (Show, Eq, Ord)

-- | Whether an option has to be defined.
data OptionRequirement
  = OptionRequired
  | OptionOptional
  deriving (Show, Eq, Ord)

emptyOption :: Option
emptyOption =
  Option
    { optionType = Nothing,
      optionDescription = Nothing,
      optionDefault = Nothing,
      optionRequirement = OptionOptional
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
      OptionTypeOneOf os -> case goEnums $ mergeSubmodules $ nubOrd $ concatMap goOr os of
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

    -- The module system merges a value of a oneOf with the first alternative
    -- whose check it passes, and that of a submodule only asks for an
    -- attribute set, so a value for any later submodule would be merged with
    -- the first one instead.
    mergeSubmodules :: [OptionType] -> [OptionType]
    mergeSubmodules ts = case mapMaybe submoduleOptions ts of
      m : ms@(_ : _) ->
        let (before, after) = break (isJust . submoduleOptions) ts
         in concat
              [ before,
                [OptionTypeSubmodule $ M.map goOpt $ foldr mergeAlternatives m ms],
                filter (isNothing . submoduleOptions) after
              ]
      _ -> ts

    submoduleOptions :: OptionType -> Maybe (Map Text Option)
    submoduleOptions = \case
      OptionTypeSubmodule m -> Just m
      OptionTypeNull -> Nothing
      OptionTypeSimple _ -> Nothing
      OptionTypeEnum _ -> Nothing
      OptionTypeNullOr _ -> Nothing
      OptionTypeListOf _ -> Nothing
      OptionTypeAttrsOf _ -> Nothing
      OptionTypeOneOf _ -> Nothing

    goOpt o = o {optionType = go <$> optionType o}

    goOr = \case
      OptionTypeOneOf os -> concatMap goOr os
      o -> [o]

simplifyOptions :: Map Text Option -> Map Text Option
simplifyOptions = M.map simplifyOption

renderOption :: Option -> Text
renderOption = renderExpression . withNixArgs . optionExpression

renderOptionType :: OptionType -> Text
renderOptionType = renderExpression . withNixArgs . optionTypeExpression

withNixArgs :: Expression -> Expression
withNixArgs = ExprFun ["lib"]

-- {-# DEPRECATED optionExpr "Use optionExpression instead" #-}
optionExpr :: Option -> Expression
optionExpr = optionExpression

optionExpression :: Option -> Expression
optionExpression o = bindDefinedSubmodule (maybeToList (optionType o)) $ unboundOptionExpression o

-- {-# DEPRECATED optionTypeExpr "Use optionTypeExpression instead" #-}
optionTypeExpr :: OptionType -> Expression
optionTypeExpr = optionTypeExpression

optionTypeExpression :: OptionType -> Expression
optionTypeExpression t = bindDefinedSubmodule [t] $ unboundOptionTypeExpression t

-- | An option that refers to 'definedSubmoduleName' without binding it.
unboundOptionExpression :: Option -> Expression
unboundOptionExpression Option {..} =
  ExprAp
    (ExprVar "lib.mkOption")
    ( ExprAttrSet $
        M.fromList $
          concat
            [ [("type", unboundOptionTypeExpression typ) | typ <- maybeToList optionType],
              [("description", ExprLitString d) | d <- maybeToList optionDescription],
              case optionDefault of
                -- The documentation would otherwise give the empty value of
                -- the type, like null for a nullOr, as the default.
                Nothing -> [("defaultText", ExprAp (ExprVar "lib.literalMD") (ExprLitString (undefaultedText optionRequirement)))]
                Just d -> [("default", toNixExpressionViaCodec d)]
            ]
    )
  where
    undefaultedText :: OptionRequirement -> Text
    undefaultedText = \case
      OptionRequired -> "None, this option is required."
      OptionOptional -> "None, this option is left out when it is not set."

-- | A type that refers to 'definedSubmoduleName' without binding it.
unboundOptionTypeExpression :: OptionType -> Expression
unboundOptionTypeExpression = go
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
        ExprAp (ExprVar definedSubmoduleName) $
          ExprAp
            (ExprVar "lib.types.submodule")
            ( ExprFun ["config", "options", "..."] $
                ExprAttrSet $
                  M.singleton "options" $
                    ExprAttrSet $
                      M.insert "__defined" (definedOptionExpression os) $
                        M.map unboundOptionExpression os
            )

-- | The option that holds what was defined in a submodule: the options that
-- were defined, and the required ones whether they were or not.
definedOptionExpression :: Map Text Option -> Expression
definedOptionExpression os =
  let (requiredKeys, optionalKeys) =
        M.mapEither
          ( \o -> case optionRequirement o of
              OptionRequired -> Left ()
              OptionOptional -> Right ()
          )
          os
      configValue :: Text -> String
      configValue key = concat ["config.", T.unpack (nixAttrKey key)]
      definedOptional :: Expression
      definedOptional =
        ExprAp
          (ExprAp (ExprVar "lib.filterAttrs") (ExprVar "(name: _: options.${name}.isDefined)"))
          (ExprAttrSet (M.mapWithKey (\key () -> ExprVar (T.pack (configValue key))) optionalKeys))
      -- Reading an option that was not defined does not throw when its type
      -- has an empty value, like null for a nullOr or [ ] for a listOf, so a
      -- required option throws itself.
      requiredValue :: Text -> String
      requiredValue key =
        let option = concat ["options.", T.unpack (nixAttrKey key)]
         in unwords
              [ "if",
                concat [option, ".isDefined"],
                "then",
                configValue key,
                "else throw",
                concat ["\"The option `${lib.showOption ", option, ".loc}' is required but was not defined.\""]
              ]
      definedRequired :: Expression
      definedRequired = ExprAttrSet (M.mapWithKey (\key () -> ExprVar (T.pack (requiredValue key))) requiredKeys)
      definedDefault :: Expression
      definedDefault
        | M.null requiredKeys = definedOptional
        | M.null optionalKeys = definedRequired
        | otherwise = ExprAp (ExprAp (ExprVar "lib.mergeAttrs") definedRequired) definedOptional
   in ExprAp (ExprVar "lib.mkOption") $
        ExprAttrSet $
          M.fromList
            [ ("internal", ExprLitBool True),
              ("readOnly", ExprLitBool True),
              ("default", definedDefault)
            ]

-- | Bind 'definedSubmoduleName' around an expression that uses any of the
-- given types, once per expression instead of once per submodule.
bindDefinedSubmodule :: [OptionType] -> Expression -> Expression
bindDefinedSubmodule ts e =
  if any hasSubmodule ts
    then
      ExprAp
        (ExprFun [definedSubmoduleName] e)
        (ExprAttrSet (M.singleton definedSubmoduleName definedSubmoduleExpression))
    else e

hasSubmodule :: OptionType -> Bool
hasSubmodule = \case
  OptionTypeNull -> False
  OptionTypeSimple _ -> False
  OptionTypeEnum _ -> False
  OptionTypeNullOr t -> hasSubmodule t
  OptionTypeListOf t -> hasSubmodule t
  OptionTypeAttrsOf t -> hasSubmodule t
  OptionTypeOneOf ts -> any hasSubmodule ts
  OptionTypeSubmodule _ -> True

definedSubmoduleName :: Text
definedSubmoduleName = "definedSubmodule"

-- | A function that makes the value of a submodule type only what was defined
-- in it, which is the value to hand to the codec.
--
-- Its merge is changed, rather than anything reading the value, so that it
-- also covers a submodule nested in any other type, because those merge their
-- elements with the element type's merge.
-- The module system rebuilds the type of an option from its submodules, so the
-- type it ends up with is made the same way.
-- Older versions of nixpkgs have no v2 merge, and no empty value for a
-- submodule.
definedSubmoduleExpression :: Expression
definedSubmoduleExpression =
  ExprVar $
    T.pack $
      unwords
        [ "let definedSubmodule = submodule: submodule // {",
          "merge = if submodule.merge ? v2",
          "then {",
          "__functor = self: loc: defs: (self.v2 { inherit loc defs; }).value;",
          "v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; };",
          "}",
          "else loc: defs: (submodule.merge loc defs).__defined;",
          "emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue;",
          "substSubModules = modules: definedSubmodule (submodule.substSubModules modules);",
          "}; in definedSubmodule"
        ]
