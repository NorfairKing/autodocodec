{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.Document where

import Autodocodec
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Generics (Generic)

-- TODO think about putting this value in a separate package or directly in autodocodec
--
-- http://json-schema.org/understanding-json-schema/reference/index.html
data JSONSchema
  = AnySchema
  | NullSchema
  | BoolSchema
  | StringSchema
  | NumberSchema
  | ArraySchema JSONSchema
  | ObjectSchema !JSONObjectSchema
  | ChoiceSchema ![JSONSchema]
  deriving (Show, Eq, Generic)

data JSONObjectSchema
  = AnyObjectSchema
  | KeySchema !Text !JSONSchema
  | BothObjectSchema !JSONObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

instance ToJSON JSONSchema where
  toJSON = \case
    AnySchema -> JSON.object []
    NullSchema -> JSON.object ["type" JSON..= ("null" :: Text)]
    BoolSchema -> JSON.object ["type" JSON..= ("boolean" :: Text)]
    StringSchema -> JSON.object ["type" JSON..= ("string" :: Text)]
    NumberSchema -> JSON.object ["type" JSON..= ("number" :: Text)]
    ArraySchema s -> JSON.object ["type" JSON..= ("array" :: Text), "items" JSON..= s]
    ObjectSchema os ->
      let go = \case
            AnyObjectSchema -> ([], [])
            KeySchema k s -> ([(k, s)], [k])
            BothObjectSchema os1 os2 ->
              let (ps1, rps1) = go os1
                  (ps2, rps2) = go os2
               in (ps1 ++ ps2, rps1 ++ rps2)
       in case go os of
            ([], _) -> JSON.object ["type" JSON..= ("object" :: Text)] -- TODO this is wrong
            (ps, []) ->
              JSON.object
                [ "type" JSON..= ("object" :: Text),
                  "properties" JSON..= ps
                ]
            (ps, rps) ->
              JSON.object
                [ "type" JSON..= ("object" :: Text),
                  "properties" JSON..= ps,
                  "required" JSON..= rps
                ]
    ChoiceSchema jcs -> JSON.object ["anyOf" JSON..= jcs]

instance FromJSON JSONSchema where
  parseJSON = JSON.withObject "JSONSchema" $ \o -> do
    mt <- o JSON..:? "type"
    case mt :: Maybe Text of
      Just "null" -> pure NullSchema
      Just "boolean" -> pure BoolSchema
      Just "string" -> pure StringSchema
      Just "number" -> pure NumberSchema
      Just "array" -> do
        mI <- o JSON..:? "items"
        case mI of
          Nothing -> pure $ ArraySchema AnySchema
          Just is -> pure $ ArraySchema is
      Just "object" -> do
        mP <- o JSON..: "properties"
        case mP of
          Nothing -> pure $ ObjectSchema AnyObjectSchema
          Just props -> do
            -- _ <- fromMaybe [] <$> o JSON..:? "required"
            -- TODO distinguish between required and optional properties
            let keySchemas = map (\(k, s) -> KeySchema k s) props
            let go (ks :| rest) = case NE.nonEmpty rest of
                  Nothing -> ks
                  Just ne -> BothObjectSchema ks (go ne)
            pure $
              ObjectSchema $ case NE.nonEmpty keySchemas of
                Nothing -> AnyObjectSchema
                Just ne -> go ne
      Nothing -> do
        mAny <- o JSON..:? "anyOf"
        case mAny of
          Just anies -> pure $ ChoiceSchema anies
          Nothing -> fail "Unknown object schema without type."
      t -> fail $ "unknown schema type:" <> show t

jsonSchemaViaCodec :: forall a. HasCodec a => JSONSchema
jsonSchemaViaCodec = jsonSchemaVia (codec @a)

jsonSchemaVia :: Codec input output -> JSONSchema
jsonSchemaVia = go
  where
    go :: Codec input output -> JSONSchema
    go = \case
      NullCodec -> NullSchema
      BoolCodec -> BoolSchema
      StringCodec -> StringSchema
      NumberCodec -> NumberSchema
      ArrayCodec c -> ArraySchema (go c)
      ObjectCodec oc -> ObjectSchema (goObject oc)
      BimapCodec _ _ c -> go c
      SelectCodec c1 c2 -> ChoiceSchema [go c1, go c2]
      ExtraParserCodec _ _ c -> go c

    goObject :: ObjectCodec input output -> JSONObjectSchema
    goObject = \case
      KeyCodec k c -> KeySchema k (go c)
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> AnyObjectSchema
      ApObjectCodec oc1 oc2 -> BothObjectSchema (goObject oc1) (goObject oc2)
