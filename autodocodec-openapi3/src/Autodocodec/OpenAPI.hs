{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.OpenAPI where

import Autodocodec
import Autodocodec.Aeson.Encode
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi as Swagger
import Data.OpenApi.Declare as Swagger
import Data.Proxy
import Data.Scientific
import Data.Text (Text)

declareNamedSchemaViaCodec :: HasCodec value => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

declareNamedSchemaVia :: JSONCodec value -> Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaVia c' Proxy = go c'
  where
    go :: ValueCodec input output -> Declare (Definitions Schema) NamedSchema
    go = \case
      NullCodec ->
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaType = Just OpenApiNull
              }
      BoolCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Bool)
      StringCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Text)
      NumberCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Scientific)
      ArrayOfCodec mname c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema mname $
            mempty
              { _schemaItems = Just $ OpenApiItemsObject $ _namedSchemaSchema <$> itemsSchemaRef,
                _schemaType = Just OpenApiArray
              }
      ObjectCodec -> declareNamedSchema (Proxy :: Proxy JSON.Object)
      ValueCodec -> pure $ NamedSchema Nothing mempty
      EqCodec val valCodec ->
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaEnum = Just [toJSONVia valCodec val]
              }
      MapCodec _ _ c -> go c
      ObjectOfCodec mname oc -> do
        ss <- goObject oc
        pure $ NamedSchema mname $ mconcat ss
      EitherCodec c1 c2 -> do
        ns1 <- go c1
        s1Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns1
        ns2 <- go c2
        s2Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns2
        pure $ NamedSchema Nothing $ mempty {_schemaAnyOf = Just [s1Ref, s2Ref]}
      CommentCodec t c -> do
        NamedSchema mName s <- go c
        let desc = case _schemaDescription s of
              Nothing -> Just t
              Just d -> Just $ t <> "\n" <> d
        pure $ NamedSchema mName $ s {_schemaDescription = desc}
      ReferenceCodec n c -> do
        mSchema <- looks (InsOrdHashMap.lookup n)
        case mSchema of
          Nothing -> do
            -- TODO this dummy idea actually doesn't work.
            declare [(n, mempty)] -- dummy
            ns <- go c
            declare [(n, _namedSchemaSchema ns)]
            pure ns
          Just s -> pure $ NamedSchema (Just n) s
    goObject :: ObjectCodec input output -> Declare (Definitions Schema) [Schema]
    goObject = \case
      RequiredKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaDescription = mDoc,
                _schemaRequired = [key],
                _schemaProperties = [(key, _namedSchemaSchema <$> ref)]
              }
          ]
      OptionalKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaDescription = mDoc, -- TODO the docs should probably go in the ns?
                _schemaProperties = [(key, _namedSchemaSchema <$> ref)]
              }
          ]
      OptionalKeyWithDefaultCodec key vs defaultValue mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaDescription = mDoc,
                _schemaProperties = [(key, _namedSchemaSchema <$> ref)],
                _schemaDefault = Just $ toJSONVia vs defaultValue
              }
          ]
      PureCodec _ -> pure []
      ApCodec oc1 oc2 -> do
        ss1 <- goObject oc1
        ss2 <- goObject oc2
        pure $ ss1 ++ ss2
      _ -> pure []

declareSpecificNamedSchemaRef :: Swagger.NamedSchema -> Declare (Definitions Schema) (Referenced NamedSchema)
declareSpecificNamedSchemaRef namedSchema =
  fmap (NamedSchema (_namedSchemaName namedSchema))
    <$> declareSpecificSchemaRef (_namedSchemaName namedSchema) (_namedSchemaSchema namedSchema)

declareSpecificSchemaRef :: Maybe Text -> Swagger.Schema -> Declare (Definitions Schema) (Referenced Schema)
declareSpecificSchemaRef mName s =
  case mName of
    Nothing -> pure $ Inline s
    Just n -> do
      known <- looks (InsOrdHashMap.member n)
      when (not known) $ declare $ InsOrdHashMap.singleton n s
      pure $ Ref (Reference n)
