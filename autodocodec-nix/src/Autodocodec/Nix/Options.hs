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
import Control.Applicative
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
-- A key is only required by a choice between objects when every alternative
-- requires it, because a value for any one alternative is a valid value.
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
                optionDefault = Nothing,
                optionRequirement = OptionRequired
              }
          )
          $ case map (go . snd) $ HM.elems m of
            [] -> M.empty
            alternative : alternatives ->
              foldl'
                ( mergeAlternatives $ \t1 t2 ->
                    t1
                      { optionType = Just $ OptionTypeOneOf $ map (fromMaybe (OptionTypeSimple "lib.types.anything") . optionType) [t1, t2],
                        optionDescription = optionDescription t1 <|> optionDescription t2, -- TODO
                        optionDefault = Nothing
                      }
                )
                alternative
                alternatives
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
      EitherCodec _ c1 c2 -> mergeAlternatives mergeOption (go c1) (go c2)
    undefaultedOption :: OptionRequirement -> ValueCodec input output -> Maybe Text -> Option
    undefaultedOption requirement c mDesc =
      Option
        { optionType = valueCodecNixOptionType c,
          optionDescription = optionDescriptionFor mDesc c,
          optionDefault = Nothing,
          optionRequirement = requirement
        }
    mergeAlternatives :: (Option -> Option -> Option) -> Map Text Option -> Map Text Option -> Map Text Option
    mergeAlternatives mergeBoth =
      Merge.merge
        (Merge.mapMissing (\_ o -> o {optionRequirement = OptionOptional}))
        (Merge.mapMissing (\_ o -> o {optionRequirement = OptionOptional}))
        ( Merge.zipWithMatched
            ( \_ o1 o2 ->
                (mergeBoth o1 o2)
                  { optionRequirement = requiredByBoth (optionRequirement o1) (optionRequirement o2)
                  }
            )
        )
    requiredByEither :: OptionRequirement -> OptionRequirement -> OptionRequirement
    requiredByEither r1 r2 = case (r1, r2) of
      (OptionOptional, OptionOptional) -> OptionOptional
      (OptionRequired, _) -> OptionRequired
      (_, OptionRequired) -> OptionRequired
    requiredByBoth :: OptionRequirement -> OptionRequirement -> OptionRequirement
    requiredByBoth r1 r2 = case (r1, r2) of
      (OptionRequired, OptionRequired) -> OptionRequired
      (OptionOptional, _) -> OptionOptional
      (_, OptionOptional) -> OptionOptional
    -- Keeping only the first description is not ideal but better than
    -- dropping both.
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
        definedSubmodule $
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
          ("default", definedDefault)
        ]
  where
    (requiredOptions, optionalOptions) = M.partition ((== OptionRequired) . optionRequirement) os
    definedOptional :: Expression
    definedOptional =
      ExprAp
        (ExprAp (ExprVar "lib.filterAttrs") (ExprVar "(name: _: options.${name}.isDefined)"))
        (ExprAttrSet (M.mapWithKey definedField optionalOptions))
    definedRequired :: Expression
    definedRequired = ExprAttrSet (M.mapWithKey definedRequiredField requiredOptions)
    definedDefault :: Expression
    definedDefault
      | M.null requiredOptions = definedOptional
      | M.null optionalOptions = definedRequired
      | otherwise = ExprAp (ExprAp (ExprVar "lib.mergeAttrs") definedRequired) definedOptional
    -- Reading an option that was not defined does not throw when its type has
    -- an empty value, like null for a nullOr or [ ] for a listOf, so a required
    -- option throws itself.
    definedRequiredField :: Text -> Option -> Expression
    definedRequiredField key _ =
      let option = concat ["options.", T.unpack (nixAttrKey key)]
       in ExprVar $
            T.pack $
              unwords
                [ "if",
                  concat [option, ".isDefined"],
                  "then",
                  concat ["config.", T.unpack (nixAttrKey key)],
                  "else throw",
                  concat ["\"The option `${lib.showOption ", option, ".loc}' is required but was not defined.\""]
                ]
    definedField :: Text -> Option -> Expression
    definedField key _ = ExprVar $ T.pack $ concat ["config.", T.unpack (nixAttrKey key)]

-- | Make the value of a submodule only what was defined in it.
--
-- Its options are only there to be type-checked and merged, and an option
-- that was not defined throws when it is read.
-- Changing the merged value of the submodule type itself, rather than having
-- consumers read @__defined@, also covers a submodule nested in any other
-- type, because those merge their elements with the element type's merge.
-- The module system rebuilds the type of an option from its submodules, so the
-- type it ends up with is made the same way.
-- Older versions of nixpkgs have no v2 merge.
definedSubmodule :: Expression -> Expression
definedSubmodule =
  ExprAp $
    ExprVar $
      T.pack $
        unwords
          [ "(let definedSubmodule = submodule: submodule // {",
            "merge = if submodule.merge ? v2",
            "then {",
            "__functor = self: loc: defs: (self.v2 { inherit loc defs; }).value;",
            "v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; };",
            "}",
            "else loc: defs: (submodule.merge loc defs).__defined;",
            "emptyValue.value = submodule.emptyValue.value.__defined;",
            "substSubModules = modules: definedSubmodule (submodule.substSubModules modules);",
            "}; in definedSubmodule)"
          ]
