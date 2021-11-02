{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Swagger where

import Autodocodec
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Proxy
import Data.Scientific
import Data.Swagger as Swagger
import Data.Swagger.Declare as Swagger
import Data.Text (Text)
import Debug.Trace

declareNamedSchemaViaCodec :: HasCodec value => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

declareNamedSchemaVia :: JSONCodec value -> Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaVia c' Proxy = go c'
  where
    go :: Codec context input output -> Declare (Definitions Schema) NamedSchema
    go = \case
      BoolCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Bool)
      StringCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Text)
      NumberCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Scientific)
      ArrayOfCodec mname c -> do
        traceM "hello"
        itemsSchema <- go c
        itemsSchemaRef <- declareNamedSchemaRef itemsSchema
        pure $
          NamedSchema mname $
            mempty
              { _schemaParamSchema =
                  mempty
                    { _paramSchemaType = Just SwaggerArray,
                      _paramSchemaItems = Just $ SwaggerItemsObject $ _namedSchemaSchema <$> itemsSchemaRef
                    }
              }
      ObjectCodec -> declareNamedSchema (Proxy :: Proxy JSON.Object)
      MapCodec _ _ c -> go c
      CommentCodec t c -> do
        NamedSchema mName s <- go c
        let desc = case _schemaDescription s of
              Nothing -> Just t
              Just d -> Just $ t <> "\n" <> d
        pure $ NamedSchema mName $ s {_schemaDescription = desc}
      _ -> pure $ NamedSchema Nothing mempty -- TODO

declareNamedSchemaRef :: Swagger.NamedSchema -> Declare (Definitions Schema) (Referenced NamedSchema)
declareNamedSchemaRef namedSchema =
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
