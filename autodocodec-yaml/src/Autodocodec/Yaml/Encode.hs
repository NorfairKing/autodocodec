{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-orphans #-}

module Autodocodec.Yaml.Encode where

import Autodocodec.Aeson.Encode
import Autodocodec.Class
import Autodocodec.Codec
import Autodocodec.DerivingVia
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as JSON
import Data.Yaml.Builder as Yaml

-- | Encode a value using its codec.
toYamlViaCodec :: HasCodec a => a -> YamlBuilder
toYamlViaCodec = toYamlVia codec

-- | Encode a value using a codec for it.
toYamlVia :: ValueCodec a void -> a -> YamlBuilder
toYamlVia = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: a -> ValueCodec a void -> YamlBuilder
    go a = \case
      NullCodec -> Yaml.null
      BoolCodec _ -> Yaml.bool (a :: Bool)
      StringCodec _ -> Yaml.string (a :: Text)
      NumberCodec _ -> yamlNumber (a :: Scientific)
      ArrayOfCodec _ c -> Yaml.array (map (`go` c) (V.toList (a :: Vector _)))
      ObjectOfCodec _ oc -> Yaml.mapping (goObject a oc)
      HashMapCodec c -> go (toJSONVia (HashMapCodec c) a) ValueCodec -- This may be optimisable?
      MapCodec c -> go (toJSONVia (MapCodec c) a) ValueCodec -- This may be optimisable?
      ValueCodec -> yamlValue (a :: JSON.Value)
      EqCodec value c -> go value c
      BimapCodec _ g c -> go (g a) c
      EitherCodec c1 c2 -> case (a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c

    goObject :: a -> ObjectCodec a void -> [(Text, YamlBuilder)]
    goObject a = \case
      RequiredKeyCodec k c _ -> [(k, go a c)]
      OptionalKeyCodec k c _ -> case (a :: Maybe _) of
        Nothing -> []
        Just b -> [k Yaml..= go b c]
      OptionalKeyWithDefaultCodec k c _ mDoc -> goObject (Just a) (OptionalKeyCodec k c mDoc)
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mDoc ->
        if a == defaultValue
          then []
          else goObject a (OptionalKeyWithDefaultCodec k c defaultValue mDoc)
      BimapCodec _ g c -> goObject (g a) c
      EitherCodec oc1 oc2 -> case (a :: Either _ _) of
        Left a1 -> goObject a1 oc1
        Right a2 -> goObject a2 oc2
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
    yamlObject a = Yaml.mapping $ HM.toList (HM.map yamlValue (a :: JSON.Object))

    -- Encode a 'JSON.Value'
    yamlValue :: JSON.Value -> YamlBuilder
    yamlValue = \case
      JSON.Null -> Yaml.null
      JSON.Bool b -> Yaml.bool b
      JSON.String s -> Yaml.string s
      JSON.Number s -> yamlNumber s
      JSON.Object o -> yamlObject o
      JSON.Array v -> Yaml.array $ map yamlValue $ V.toList v

instance HasCodec a => ToYaml (Autodocodec a) where
  toYaml = toYamlViaCodec . unAutodocodec
