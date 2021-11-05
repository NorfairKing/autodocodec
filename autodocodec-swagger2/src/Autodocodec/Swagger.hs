{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Swagger where

import Autodocodec
import Autodocodec.Aeson.Encode
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.List
import Data.Proxy
import Data.Scientific
import Data.Swagger as Swagger
import Data.Swagger.Declare as Swagger
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
              { _schemaParamSchema =
                  mempty
                    { _paramSchemaType = Just SwaggerNull
                    }
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
              { _schemaParamSchema =
                  mempty
                    { _paramSchemaType = Just SwaggerArray,
                      _paramSchemaItems = Just $ SwaggerItemsObject $ _namedSchemaSchema <$> itemsSchemaRef
                    }
              }
      ObjectOfCodec mname oc -> do
        ss <- goObject oc
        pure $ NamedSchema mname $ mconcat ss
      ObjectCodec -> declareNamedSchema (Proxy :: Proxy JSON.Object)
      ValueCodec -> pure $ NamedSchema Nothing mempty -- TODO
      EqCodec val valCodec ->
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaParamSchema = mempty {_paramSchemaEnum = Just [toJSONVia valCodec val]}
              }
      MapCodec _ _ c -> go c
      -- Swagger 2 doesn't support sum types so we have to work around that here.
      EitherCodec c1 c2 -> do
        ns1 <- go c1
        let s1 = _namedSchemaSchema ns1
        let ps1 = _schemaParamSchema s1
        ns2 <- go c2
        let s2 = _namedSchemaSchema ns2
        let ps2 = _schemaParamSchema s2
        pure $
          NamedSchema Nothing $
            (s1 <> s2)
              { _schemaRequired =
                  _schemaRequired s1
                    `intersect` _schemaRequired s2,
                _schemaParamSchema =
                  mempty
                    { _paramSchemaEnum =
                        liftA2
                          (++)
                          (_paramSchemaEnum ps1)
                          (_paramSchemaEnum ps2)
                    }
                    -- TODO all the other bits and pieces.
              }
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
                _schemaParamSchema = mempty {_paramSchemaDefault = Just $ toJSONVia vs defaultValue}
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
