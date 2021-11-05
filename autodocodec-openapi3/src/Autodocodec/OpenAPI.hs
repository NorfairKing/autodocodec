{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.OpenAPI where

import Autodocodec
import Autodocodec.Aeson.Encode
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi as OpenAPI
import Data.OpenApi.Declare as OpenAPI
import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Typeable

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
        let s1 = _namedSchemaSchema ns1
        s1Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns1
        ns2 <- go c2
        let s2 = _namedSchemaSchema ns2
        s2Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns2
        pure $
          NamedSchema Nothing $ case (_schemaAnyOf s1, _schemaAnyOf s2) of
            (Just s1s, Just s2s) -> mempty {_schemaAnyOf = Just $ s1s ++ s2s}
            (Just s1s, Nothing) -> mempty {_schemaAnyOf = Just $ s1s ++ [s2Ref]}
            (Nothing, Just s2s) -> mempty {_schemaAnyOf = Just $ s1Ref : s2s}
            (Nothing, Nothing) -> mempty {_schemaAnyOf = Just [s1Ref, s2Ref]}
      CommentCodec t c -> do
        NamedSchema mName s <- go c
        pure $ NamedSchema mName $ addDoc t s
      ReferenceCodec n c -> do
        mSchema <- looks (InsOrdHashMap.lookup n)
        case mSchema of
          Nothing -> do
            d <- look
            let (d', ns) = runDeclare (go c) (InsOrdHashMap.insert n mempty d)
            declare $ InsOrdHashMap.insert n (_namedSchemaSchema ns) d'
            pure ns
          Just s -> pure $ NamedSchema (Just n) s
    goObject :: ObjectCodec input output -> Declare (Definitions Schema) [Schema]
    goObject = \case
      RequiredKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaRequired = [key],
                _schemaProperties = [(key, addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaType = Just OpenApiObject
              }
          ]
      OptionalKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaType = Just OpenApiObject
              }
          ]
      OptionalKeyWithDefaultCodec key vs defaultValue mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaDefault = Just $ toJSONVia vs defaultValue,
                _schemaType = Just OpenApiObject
              }
          ]
      PureCodec _ -> pure []
      ApCodec oc1 oc2 -> do
        ss1 <- goObject oc1
        ss2 <- goObject oc2
        pure $ ss1 ++ ss2
      MapCodec _ _ oc -> goObject oc
    addMDoc :: Maybe Text -> Schema -> Schema
    addMDoc = maybe id addDoc
    addDoc :: Text -> Schema -> Schema
    addDoc doc s =
      s
        { _schemaDescription = case _schemaDescription s of
            Nothing -> Just doc
            Just doc' -> Just $ doc <> "\n" <> doc'
        }

declareSpecificNamedSchemaRef :: OpenAPI.NamedSchema -> Declare (Definitions Schema) (Referenced NamedSchema)
declareSpecificNamedSchemaRef namedSchema =
  fmap (NamedSchema (_namedSchemaName namedSchema))
    <$> declareSpecificSchemaRef (_namedSchemaName namedSchema) (_namedSchemaSchema namedSchema)

declareSpecificSchemaRef :: Maybe Text -> OpenAPI.Schema -> Declare (Definitions Schema) (Referenced Schema)
declareSpecificSchemaRef mName s =
  case mName of
    Nothing -> pure $ Inline s
    Just n -> do
      known <- looks (InsOrdHashMap.member n)
      when (not known) $ declare $ InsOrdHashMap.singleton n s
      pure $ Ref (Reference n)

instance (Typeable a, HasCodec a) => OpenAPI.ToSchema (Autodocodec a) where
  declareNamedSchema (Proxy :: Proxy (Autodocodec a)) = declareNamedSchemaViaCodec (Proxy :: Proxy a)
