{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.Document where

import Autodocodec
import Autodocodec.Aeson.Encode
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Validity
import Data.Validity.Aeson ()
import Data.Validity.Containers ()
import Data.Validity.Text ()
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
  | ArraySchema !JSONSchema
  | ObjectSchema !(Map Text (KeyRequirement, JSONSchema)) -- TODO it's important (for docs) that these stay ordered.
  | ValueSchema !JSON.Value
  | ChoiceSchema !(NonEmpty JSONSchema)
  | CommentSchema !Text !JSONSchema
  deriving (Show, Eq, Generic)

instance Validity JSONSchema where
  validate js =
    mconcat
      [ genericValidate js,
        declare "never has two nested comments" $ case js of
          CommentSchema _ (CommentSchema _ _) -> False
          _ -> True,
        case js of
          ChoiceSchema cs -> declare "there are 2 of more choices" $ length cs >= 2
          _ -> valid
      ]

data KeyRequirement = Required | Optional
  deriving (Show, Eq, Generic)

instance Validity KeyRequirement

instance ToJSON JSONSchema where
  toJSON = JSON.object . go
    where
      go = \case
        AnySchema -> []
        NullSchema -> ["type" JSON..= ("null" :: Text)]
        BoolSchema -> ["type" JSON..= ("boolean" :: Text)]
        StringSchema -> ["type" JSON..= ("string" :: Text)]
        NumberSchema -> ["type" JSON..= ("number" :: Text)]
        ArraySchema s -> ["type" JSON..= ("array" :: Text), "items" JSON..= s]
        ValueSchema v -> ["const" JSON..= v]
        ObjectSchema os ->
          let combine (ps, rps) k (r, s) =
                ( (k, s) : ps,
                  case r of
                    Required -> k : rps
                    Optional -> rps
                )
           in case M.foldlWithKey combine ([], []) os of
                ([], _) -> ["type" JSON..= ("object" :: Text)]
                (ps, []) ->
                  [ "type" JSON..= ("object" :: Text),
                    "properties" JSON..= HM.fromList ps
                  ]
                (ps, rps) ->
                  [ "type" JSON..= ("object" :: Text),
                    "properties" JSON..= HM.fromList ps,
                    "required" JSON..= rps
                  ]
        ChoiceSchema jcs -> ["anyOf" JSON..= jcs]
        CommentSchema comment s -> ("$comment" JSON..= comment) : go s -- TODO this is probably wrong.

instance FromJSON JSONSchema where
  parseJSON = JSON.withObject "JSONSchema" $ \o -> do
    mt <- o JSON..:? "type"
    mc <- o JSON..:? "$comment"
    let commentFunc = maybe id CommentSchema mc
    fmap commentFunc $ case mt :: Maybe Text of
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
        mP <- o JSON..:? "properties"
        case mP of
          Nothing -> pure $ ObjectSchema M.empty
          Just (props :: Map Text JSONSchema) -> do
            requiredProps <- fromMaybe [] <$> o JSON..:? "required"
            let keySchemaFor k s =
                  M.singleton
                    k
                    ( if k `elem` requiredProps
                        then Required
                        else Optional,
                      s
                    )
            pure $ ObjectSchema $ M.unions $ map (uncurry keySchemaFor) $ M.toList props
      Nothing -> do
        mAny <- o JSON..:? "anyOf"
        case mAny of
          Just anies -> pure $ ChoiceSchema anies
          Nothing -> do
            mConst <- o JSON..:? "const"
            pure $ case mConst of
              Just constant -> ValueSchema constant
              Nothing -> AnySchema
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
      ArrayCodec mname c -> maybe id CommentSchema mname $ ArraySchema (go c)
      ObjectCodec mname oc -> maybe id CommentSchema mname $ ObjectSchema (goObject oc)
      EqCodec value c -> ValueSchema (toJSONVia c value)
      BimapCodec _ _ c -> go c
      EitherCodec c1 c2 -> ChoiceSchema (goChoice (go c1 :| [go c2]))
      ExtraParserCodec _ _ c -> go c
      CommentCodec t c -> CommentSchema t (go c)

    goChoice :: NonEmpty JSONSchema -> NonEmpty JSONSchema
    goChoice (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goChoice ne
      where
        goSingle :: JSONSchema -> NonEmpty JSONSchema
        goSingle = \case
          ChoiceSchema ss -> goChoice ss
          s' -> s' :| []

    goObject :: ObjectCodec input output -> Map Text (KeyRequirement, JSONSchema)
    goObject = \case
      RequiredKeyCodec k c -> M.singleton k (Required, go c)
      OptionalKeyCodec k c -> M.singleton k (Optional, go c)
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> M.empty
      ApObjectCodec oc1 oc2 -> M.union (goObject oc1) (goObject oc2)
