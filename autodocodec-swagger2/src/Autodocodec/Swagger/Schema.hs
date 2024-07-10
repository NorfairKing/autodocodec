{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.Swagger.Schema where

import Autodocodec
import Control.Monad
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.List
import Data.Proxy
import Data.Scientific
import Data.Swagger as Swagger
import Data.Swagger.Declare as Swagger
import Data.Text (Text)

-- | Use a type's 'codec' to implement 'declareNamedSchema'.
declareNamedSchemaViaCodec :: (HasCodec value) => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

-- | Use a given 'codec' to implement 'declareNamedSchema'.
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
      NumberCodec mname mBounds -> do
        s <- declareSchema (Proxy :: Proxy Scientific)
        let addNumberBounds NumberBounds {..} s_ =
              s_
                { _schemaParamSchema =
                    (_schemaParamSchema s_)
                      { _paramSchemaMinimum = Just numberBoundsLower,
                        _paramSchemaMaximum = Just numberBoundsUpper
                      }
                }
        pure $ NamedSchema mname $ maybe id addNumberBounds mBounds s
      HashMapCodec c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema $ _namedSchemaSchema <$> itemsSchemaRef,
                _schemaParamSchema =
                  mempty
                    { _paramSchemaType = Just SwaggerObject
                    }
              }
      MapCodec c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema $ _namedSchemaSchema <$> itemsSchemaRef,
                _schemaParamSchema =
                  mempty
                    { _paramSchemaType = Just SwaggerObject
                    }
              }
      ValueCodec ->
        pure $
          NamedSchema
            Nothing
            mempty
              { _schemaAdditionalProperties = Just $ AdditionalPropertiesAllowed True
              }
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
        pure $ NamedSchema mname $ combineObjectSchemas ss
      EqCodec val valCodec ->
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaParamSchema = mempty {_paramSchemaEnum = Just [toJSONVia valCodec val]}
              }
      BimapCodec _ _ c -> go c
      EitherCodec u c1 c2 -> do
        ns1 <- go c1
        let s1 = _namedSchemaSchema ns1
        ns2 <- go c2
        let s2 = _namedSchemaSchema ns2
        pure $ NamedSchema Nothing $ combineSchemaOr u s1 s2
      CommentCodec t c -> do
        NamedSchema mName s <- go c
        pure $ NamedSchema mName $ addDoc t s
      ReferenceCodec n c -> do
        d <- look
        case InsOrdHashMap.lookup n d of
          Nothing -> do
            -- Insert a dummy to prevent an infinite loop.
            let dummy = mempty
            let (d', ns) = runDeclare (go c) (InsOrdHashMap.insert n dummy d)
            -- Override the dummy once we actually know what the result will be.
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
                _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
              }
          ]
      OptionalKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
              }
          ]
      OptionalKeyWithDefaultCodec key vs defaultValue mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaParamSchema =
                  mempty
                    { _paramSchemaDefault = Just $ toJSONVia vs defaultValue,
                      _paramSchemaType = Just SwaggerObject
                    }
              }
          ]
      OptionalKeyWithOmittedDefaultCodec key vs defaultValue mDoc -> goObject (optionalKeyWithDefaultCodec key vs defaultValue mDoc)
      PureCodec _ -> pure []
      EitherCodec u oc1 oc2 -> do
        ss1 <- goObject oc1
        ss2 <- goObject oc2
        pure [combineSchemaOr u (combineObjectSchemas ss1) (combineObjectSchemas ss2)]
      DiscriminatedUnionCodec pn _ m -> do
        let mkSchema dName (_, oc) =
              fmap combineObjectSchemas $ goObject $ oc *> (requiredFieldWith' pn textCodec .= const dName)
        ss <- HashMap.traverseWithKey mkSchema m
        let combined = case toList ss of
              [] -> mempty
              (s : ss') -> foldr (combineSchemaOr DisjointUnion) s ss'
        pure [combined]
      ApCodec oc1 oc2 -> do
        ss1 <- goObject oc1
        ss2 <- goObject oc2
        pure $ ss1 ++ ss2
      BimapCodec _ _ oc -> goObject oc
    addMDoc :: Maybe Text -> Schema -> Schema
    addMDoc = maybe id addDoc
    addDoc :: Text -> Schema -> Schema
    addDoc doc s =
      s
        { _schemaDescription = case _schemaDescription s of
            Nothing -> Just doc
            Just doc' -> Just $ doc <> "\n" <> doc'
        }
    combineObjectSchemas :: [Schema] -> Schema
    combineObjectSchemas = mconcat
    combineSchemaOr :: Union -> Schema -> Schema -> Schema
    combineSchemaOr u s1 s2 =
      let ps1 = _schemaParamSchema s1
          ps2 = _schemaParamSchema s2
          -- Swagger 2 doesn't support sum types so we have to work around that here.
          --
          -- We support a few cases:
          --
          --   * Enum: If both of them have the enum field set
          --   * Two obects: If both of them have the object type set.
          --
          -- If none of these cases match, we just take an overapproximation of
          -- the schema: one which lets any value through.
          overApproximation =
            mempty
              { _schemaAdditionalProperties = case u of
                  PossiblyJointUnion -> Just $ AdditionalPropertiesAllowed True
                  DisjointUnion -> Nothing
              }
       in case (,) <$> _paramSchemaEnum ps1 <*> _paramSchemaEnum ps2 of
            (Just (es1, es2)) ->
              mempty
                { _schemaParamSchema =
                    mempty
                      { _paramSchemaEnum = Just $ es1 ++ es2,
                        _paramSchemaType =
                          case (,) <$> _paramSchemaType ps1 <*> _paramSchemaType ps2 of
                            Just (t1, t2)
                              | t1 == t2 -> Just t1
                              | otherwise -> Nothing
                            Nothing -> Nothing
                      }
                }
            Nothing ->
              case (,) <$> _paramSchemaType ps1 <*> _paramSchemaType ps2 of
                Just (SwaggerObject, SwaggerObject) ->
                  mempty
                    { _schemaRequired = _schemaRequired s1 `intersect` _schemaRequired s2,
                      _schemaProperties = InsOrdHashMap.union (_schemaProperties s1) (_schemaProperties s2),
                      _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
                    }
                Just (a, b)
                  | a == b -> mempty {_schemaParamSchema = mempty {_paramSchemaType = Just a}}
                  | otherwise -> overApproximation
                _ -> overApproximation

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
