{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Autodocodec.Yaml.Encode where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Aeson.Encode
import Autodocodec.Class
import Autodocodec.Codec
import Control.Arrow (first)
import Data.Coerce (coerce)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as JSON
import Data.Yaml.Builder as Yaml

-- | Implement 'Yaml.toYaml' using a type's codec
toYamlViaCodec :: (HasCodec a) => a -> YamlBuilder
toYamlViaCodec = toYamlVia codec

-- | Implement 'Yaml.toYaml' using a given codec
toYamlVia :: ValueCodec a void -> a -> YamlBuilder
toYamlVia = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: a -> ValueCodec a void -> YamlBuilder
    go a = \case
      NullCodec -> Yaml.null
      BoolCodec _ -> Yaml.bool (coerce a :: Bool)
      StringCodec _ -> Yaml.string (coerce a :: Text)
      IntegerCodec _ _ -> Yaml.scientific $ fromInteger (coerce a :: Integer)
      NumberCodec _ _ -> yamlNumber (coerce a :: Scientific)
      ArrayOfCodec _ c -> Yaml.array (map (`go` c) (V.toList (coerce a :: Vector _)))
      ObjectOfCodec _ oc -> Yaml.mapping (goObject a oc)
      c@(HashMapCodec {}) -> go (toJSONVia c a) valueCodec -- This may be optimisable?
      c@(MapCodec {}) -> go (toJSONVia c a) valueCodec -- This may be optimisable?
      ValueCodec -> yamlValue (coerce a :: JSON.Value)
      EqCodec value c -> go value c
      BimapCodec _ g c -> go (g a) c
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c

    goObject :: a -> ObjectCodec a void -> [(Text, YamlBuilder)]
    goObject a = \case
      RequiredKeyCodec k c _ -> [(k, go (coerce a) c)]
      OptionalKeyCodec k c _ -> case (coerce a :: Maybe _) of
        Nothing -> []
        Just b -> [k Yaml..= go b c]
      OptionalKeyWithDefaultCodec k c _ mDoc -> goObject (Just a) (optionalKeyCodec k c mDoc)
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mDoc ->
        if coerce a == defaultValue
          then []
          else goObject a (optionalKeyWithDefaultCodec k (coerce c) (coerce defaultValue) mDoc)
      BimapCodec _ g c -> goObject (g a) c
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> goObject a1 c1
        Right a2 -> goObject a2 c2
      DiscriminatedUnionCodec propertyName m _ ->
        case m a of
          (discriminatorValue, c) ->
            (propertyName, Yaml.string discriminatorValue) : goObject a c
      PureCodec _ -> []
      ApCodec oc1 oc2 -> goObject a oc1 <> goObject a oc2

    -- Encode a 'Scientific' value 'safely' by refusing to encode values that would be enormous.
    yamlNumber :: Scientific -> YamlBuilder
    yamlNumber s =
      if s > 1E1024 || s < -1E1024
        then Yaml.string $ "Cannot encode super duper large numbers with toYaml: " <> T.pack (show s)
        else Yaml.scientific s

    -- Encode a 'JSON.Object'
    yamlObject :: JSON.Object -> YamlBuilder
    yamlObject a = Yaml.mapping $ map (first Compat.fromKey) $ Compat.toList (Compat.map yamlValue (a :: JSON.Object))

    -- Encode a 'JSON.Value'
    yamlValue :: JSON.Value -> YamlBuilder
    yamlValue = \case
      JSON.Null -> Yaml.null
      JSON.Bool b -> Yaml.bool b
      JSON.String s -> Yaml.string s
      JSON.Number s -> yamlNumber s
      JSON.Object o -> yamlObject o
      JSON.Array v -> Yaml.array $ map yamlValue $ V.toList v

newtype AutodocodecYaml a = AutodocodecYaml {unAutodocodecYaml :: a}

instance (HasCodec a) => ToYaml (AutodocodecYaml a) where
  toYaml = toYamlViaCodec . unAutodocodecYaml
