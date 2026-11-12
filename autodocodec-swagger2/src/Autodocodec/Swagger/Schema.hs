{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Text as T

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
      StringCodec mname mBounds -> do
        s <- declareSchema (Proxy :: Proxy Text)
        let addStringBounds StringBounds {..} s_ =
              s_
                { _schemaParamSchema =
                    (_schemaParamSchema s_)
                      { _paramSchemaMinLength = fromIntegral <$> stringBoundsMinLength,
                        _paramSchemaMaxLength = fromIntegral <$> stringBoundsMaxLength
                      }
                }
        pure $ NamedSchema mname $ addStringBounds mBounds s
      IntegerCodec mname mBounds -> do
        s <- declareSchema (Proxy :: Proxy Integer)
        let addNumberBounds Bounds {..} s_ =
              s_
                { _schemaParamSchema =
                    (_schemaParamSchema s_)
                      { _paramSchemaMinimum = fromInteger <$> boundsLower,
                        _paramSchemaMaximum = fromInteger <$> boundsUpper
                      }
                }
        pure $ NamedSchema mname $ addNumberBounds mBounds s
      NumberCodec mname mBounds -> do
        s <- declareSchema (Proxy :: Proxy Scientific)
        let addNumberBounds Bounds {..} s_ =
              s_
                { _schemaParamSchema =
                    (_schemaParamSchema s_)
                      { _paramSchemaMinimum = boundsLower,
                        _paramSchemaMaximum = boundsUpper
                      }
                }
        pure $ NamedSchema mname $ addNumberBounds mBounds s
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
      ExtraDocsCodec docs c -> do
        NamedSchema mName s <- go c
        pure $ NamedSchema mName $ applyExtraDocs docs s
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
                _schemaProperties = [(key, fieldProperty mDoc ref)],
                _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
              }
          ]
      OptionalKeyCodec key vs mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, fieldProperty mDoc ref)],
                _schemaParamSchema = mempty {_paramSchemaType = Just SwaggerObject}
              }
          ]
      OptionalKeyWithDefaultCodec key vs defaultValue mDoc -> do
        ns <- go vs
        ref <- declareSpecificNamedSchemaRef ns
        pure
          [ mempty
              { _schemaProperties = [(key, fieldProperty mDoc ref)],
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
    -- A field's documentation is a description on top of its value's schema.
    addMDoc :: Maybe Text -> Schema -> Schema
    addMDoc = maybe id (applyExtraDocs . ExtraDescription)
    -- The property schema for an object field: its value's schema, plus the
    -- field's own doc.
    --
    -- When the value's type has no name, its schema is inline, and the field's
    -- doc goes straight onto it.
    --
    -- When the value's type is named, its schema is a '$ref' to a definition
    -- that every use of the type shares.  The field's doc is about this one
    -- field, so it can't go in that definition, or every other use of the type
    -- would show it too.  It has to go at the '$ref' instead, but Swagger 2
    -- ignores anything next to a '$ref'.  So we wrap the '$ref' in a
    -- one-element 'allOf', which is a Schema Object and can carry it:
    --
    -- > "field": { "allOf": [{ "$ref": "#/definitions/T" }], "description": "..." }
    --
    -- swagger2's 'Referenced' type has nowhere to put anything next to a 'Ref'
    -- anyway.  Unlike OpenAPI, where 3.1 lets keywords sit next to a '$ref'
    -- (see 'Autodocodec.OpenAPI.Schema'), there is no later Swagger 2 spec that
    -- relaxes this, so the wrapper stays for as long as this interpreter exists.
    --
    -- An empty doc counts as no doc here, because a wrapper with an empty
    -- description says nothing.
    fieldProperty :: Maybe Text -> Referenced NamedSchema -> Referenced Schema
    fieldProperty mDoc ref = case _namedSchemaSchema <$> ref of
      Inline s -> Inline $ addMDoc mDoc s
      r@(Ref _) -> case mfilter (not . T.null) mDoc of
        Nothing -> r
        mDoc' -> Inline $ addMDoc mDoc' mempty {_schemaAllOf = Just [r]}
    -- Apply a doc that wraps the codec that this schema came from.
    -- See 'ExtraDocs' for how it combines with what the schema already has.
    applyExtraDocs :: ExtraDocs -> Schema -> Schema
    applyExtraDocs docs s =
      let ps = _schemaParamSchema s
       in case docs of
            ExtraDescription doc ->
              s
                { _schemaDescription = case _schemaDescription s of
                    Nothing -> Just doc
                    Just doc' -> Just $ doc <> "\n" <> doc'
                }
            ExtraTitle t -> s {_schemaTitle = Just t}
            ExtraExample e -> s {_schemaExample = Just e}
            ExtraFormat f -> s {_schemaParamSchema = ps {_paramSchemaFormat = Just f}}
            ExtraMultipleOf m -> s {_schemaParamSchema = ps {_paramSchemaMultipleOf = Just m}}
            -- Both constraints apply to the same array, so the stricter one holds.
            ExtraMinItems n -> s {_schemaParamSchema = ps {_paramSchemaMinItems = Just $ maybe (fromIntegral n) (max (fromIntegral n)) (_paramSchemaMinItems ps)}}
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
