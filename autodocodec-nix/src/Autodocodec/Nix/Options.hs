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
--
-- A generated submodule uses an internal option named @__defined@, so a codec
-- with a key of that name cannot be rendered: evaluating its type throws.
-- The option that a generated submodule is the type of cannot be declared
-- again, because the module system would merge the declarations into a plain
-- submodule, whose value is not what was defined.
objectCodecNixOptions :: ObjectCodec input output -> Map Text Option
objectCodecNixOptions =
  let undefaultedOption :: OptionRequirement -> ValueCodec input output -> Maybe Text -> Option
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
   in simplifyOptions . go

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
  let undefaultedText :: Text
      undefaultedText = case optionRequirement of
        OptionRequired -> "None, this option is required."
        OptionOptional -> "None, this option is left out when it is not set."
   in ExprAp
        (ExprVar "lib.mkOption")
        ( ExprAttrSet $
            M.fromList $
              concat
                [ [("type", unboundOptionTypeExpression typ) | typ <- maybeToList optionType],
                  [("description", ExprLitString d) | d <- maybeToList optionDescription],
                  case optionDefault of
                    -- The documentation would otherwise give the empty value
                    -- of the type, like null for a nullOr, as the default.
                    Nothing -> [("defaultText", ExprAp (ExprVar "lib.literalMD") (ExprLitString undefaultedText))]
                    Just d -> [("default", toNixExpressionViaCodec d)]
                ]
        )

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
        let requiredKey :: Text -> Option -> Maybe Expression
            requiredKey key o = case optionRequirement o of
              OptionRequired -> Just (ExprLitString key)
              OptionOptional -> Nothing
         in ExprAp
              (ExprAp (ExprVar definedSubmoduleName) (ExprLitList (M.elems (M.mapMaybeWithKey requiredKey os))))
              (ExprAttrSet (M.map unboundOptionExpression os))

-- | Bind 'definedSubmoduleName' around an expression that uses any of the
-- given types, once per expression instead of once per submodule.
bindDefinedSubmodule :: [OptionType] -> Expression -> Expression
bindDefinedSubmodule ts e =
  if any hasSubmodule ts
    then
      ExprLet
        ( M.fromList
            [ (definedSubmoduleName, definedSubmoduleExpression),
              (definedTypeName, definedTypeExpression)
            ]
        )
        e
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

definedTypeName :: Text
definedTypeName = "definedType"

-- | The name of the internal option that holds what was defined in a
-- generated submodule.
-- A codec with a key of this name cannot be rendered as a submodule, so the
-- generated type throws when it is evaluated.
definedOptionName :: Text
definedOptionName = "__defined"

-- | A function from the required keys and the option declarations of a
-- submodule to a submodule type whose value is what was defined in it: the
-- options that were defined, and the required ones whether they were or not.
--
-- Reading an option that was not defined does not throw when its type has an
-- empty value, like null for a nullOr or [ ] for a listOf, so a required
-- option throws itself.
definedSubmoduleExpression :: Expression
definedSubmoduleExpression =
  let getAttr :: Text -> Expression
      getAttr set = ExprAp (ExprAp (ExprVar "lib.getAttr") (ExprVar "name")) (ExprVar set)
      isDefined :: Expression
      isDefined = ExprSelect (getAttr "options") "isDefined"
      requiredValue :: Expression
      requiredValue =
        ExprIf
          isDefined
          (getAttr "config")
          ( ExprAp
              (ExprVar "throw")
              ( ExprAp
                  (ExprVar "lib.concatStrings")
                  ( ExprLitList
                      [ ExprLitString "The option `",
                        ExprAp (ExprVar "lib.showOption") (ExprSelect (getAttr "options") "loc"),
                        ExprLitString "' is required but was not defined."
                      ]
                  )
              )
          )
      defined :: Expression
      defined =
        ExprUpdate
          (ExprAp (ExprAp (ExprVar "lib.genAttrs") (ExprVar "required")) (ExprLam "name" requiredValue))
          ( ExprAp
              (ExprAp (ExprVar "lib.mapAttrs") (ExprLam "name" (ExprLam "_" (getAttr "config"))))
              (ExprAp (ExprAp (ExprVar "lib.filterAttrs") (ExprLam "name" (ExprLam "_" isDefined))) (ExprVar "declarations"))
          )
      definedOption :: Expression
      definedOption =
        ExprAp (ExprVar "lib.mkOption") $
          ExprAttrSet $
            M.fromList
              [ ("internal", ExprLitBool True),
                ("readOnly", ExprLitBool True),
                ("default", defined)
              ]
      submoduleModule :: Expression
      submoduleModule =
        ExprFun ["config", "options", "..."] $
          ExprAttrSet $
            M.singleton "options" $
              ExprUpdate (ExprVar "declarations") (ExprAttrSet (M.singleton definedOptionName definedOption))
   in ExprLam "required" $
        ExprLam "declarations" $
          ExprAp
            ( ExprAp
                ( ExprAp
                    (ExprVar "lib.throwIf")
                    (ExprAp (ExprAp (ExprVar "lib.hasAttr") (ExprLitString definedOptionName)) (ExprVar "declarations"))
                )
                (ExprLitString (T.unwords ["autodocodec-nix uses the option name", definedOptionName, "itself."]))
            )
            (ExprAp (ExprVar definedTypeName) (ExprAp (ExprVar "lib.types.submodule") submoduleModule))

-- | A function that makes the value of a submodule type the value of its
-- 'definedOptionName' option.
--
-- Its merge is changed, rather than anything reading the value, so that it
-- also covers a submodule nested in any other type, because those merge their
-- elements with the element type's merge.
-- The module system rebuilds the type of an option from its submodules, so the
-- type it ends up with is made the same way.
-- It cannot be merged with another declaration of the same option, because
-- the result would be a plain submodule, whose value is not what was defined.
-- Its functor has a name of its own so that a plain submodule cannot be merged
-- with it either.
-- Older versions of nixpkgs have no v2 merge, and no empty value for a
-- submodule.
definedTypeExpression :: Expression
definedTypeExpression =
  let submodule :: Text -> Expression
      submodule = ExprSelect (ExprVar "submodule")
      definedOf :: Expression -> Expression
      definedOf e = ExprSelect e definedOptionName
      v2Merge :: Expression
      v2Merge =
        ExprAttrSet $
          M.fromList
            [ ( "__functor",
                ExprLam "self" $
                  ExprLam "loc" $
                    ExprLam "defs" $
                      ExprSelect
                        ( ExprAp
                            (ExprSelect (ExprVar "self") "v2")
                            (ExprAttrSet (M.fromList [("loc", ExprVar "loc"), ("defs", ExprVar "defs")]))
                        )
                        "value"
              ),
              ( "v2",
                ExprLam "args" $
                  ExprLet
                    (M.singleton "merged" (ExprAp (ExprSelect (submodule "merge") "v2") (ExprVar "args")))
                    ( ExprUpdate
                        (ExprVar "merged")
                        (ExprAttrSet (M.singleton "value" (definedOf (ExprSelect (ExprVar "merged") "value"))))
                    )
              )
            ]
      v1Merge :: Expression
      v1Merge =
        ExprLam "loc" $
          ExprLam "defs" $
            definedOf (ExprAp (ExprAp (submodule "merge") (ExprVar "loc")) (ExprVar "defs"))
   in ExprLam "submodule" $
        ExprUpdate (ExprVar "submodule") $
          ExprAttrSet $
            M.fromList
              [ ("merge", ExprIf (ExprAp (ExprVar "lib.isAttrs") (submodule "merge")) v2Merge v1Merge),
                ( "emptyValue",
                  ExprIf
                    (ExprAp (ExprAp (ExprVar "lib.hasAttr") (ExprLitString "value")) (submodule "emptyValue"))
                    (ExprAttrSet (M.singleton "value" (definedOf (ExprSelect (submodule "emptyValue") "value"))))
                    (submodule "emptyValue")
                ),
                ( "substSubModules",
                  ExprLam "modules" $
                    ExprAp (ExprVar definedTypeName) (ExprAp (submodule "substSubModules") (ExprVar "modules"))
                ),
                ("typeMerge", ExprLam "_" ExprNull),
                ("functor", ExprUpdate (submodule "functor") (ExprAttrSet (M.singleton "name" (ExprLitString definedSubmoduleName))))
              ]
