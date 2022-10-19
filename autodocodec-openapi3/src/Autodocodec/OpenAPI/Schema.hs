{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.OpenAPI.Schema where

import Autodocodec
import Control.Lens (Lens', (&), (.~), (?~), (^.))
import Control.Monad
import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi as OpenAPI
import Data.OpenApi.Declare as OpenAPI
import Data.Proxy
import Data.Scientific
import Data.Text (Text)

-- | Use a type's 'codec' to implement 'declareNamedSchema'.
declareNamedSchemaViaCodec :: HasCodec value => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

-- | Use a given 'codec' to implement 'declareNamedSchema'.
declareNamedSchemaVia :: JSONCodec value -> Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaVia c' Proxy = evalStateT (go c') mempty
  where
    go :: ValueCodec input output -> StateT (HashMap Text Schema) (Declare (Definitions Schema)) NamedSchema
    go = \case
      NullCodec ->
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaType = Just OpenApiNull
              }
      BoolCodec mname -> lift $ NamedSchema mname <$> declareSchema (Proxy :: Proxy Bool)
      StringCodec mname -> lift $ NamedSchema mname <$> declareSchema (Proxy :: Proxy Text)
      NumberCodec mname mBounds -> do
        s <- lift $ declareSchema (Proxy :: Proxy Scientific)
        let addNumberBounds NumberBounds {..} s_ =
              s_
                { _schemaMinimum = Just numberBoundsLower,
                  _schemaMaximum = Just numberBoundsUpper
                }
        pure $ NamedSchema mname $ maybe id addNumberBounds mBounds s
      ArrayOfCodec mname c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema mname $
            mempty
              { _schemaItems = Just $ OpenApiItemsObject $ _namedSchemaSchema <$> itemsSchemaRef,
                _schemaType = Just OpenApiArray
              }
      HashMapCodec c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaType = Just OpenApiObject,
                _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema $ _namedSchemaSchema <$> itemsSchemaRef
              }
      MapCodec c -> do
        itemsSchema <- go c
        itemsSchemaRef <- declareSpecificNamedSchemaRef itemsSchema
        pure $
          NamedSchema Nothing $
            mempty
              { _schemaType = Just OpenApiObject,
                _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema $ _namedSchemaSchema <$> itemsSchemaRef
              }
      ValueCodec ->
        pure $
          NamedSchema
            Nothing
            mempty
              { _schemaAdditionalProperties = Just $ AdditionalPropertiesAllowed True
              }
      EqCodec val valCodec ->
        pure $
          NamedSchema Nothing $
            let jsonVal = toJSONVia valCodec val
             in mempty
                  { _schemaEnum = Just [jsonVal],
                    _schemaType = Just $ case jsonVal of
                      Aeson.Object {} -> OpenApiObject
                      Aeson.Array {} -> OpenApiArray
                      Aeson.String {} -> OpenApiString
                      Aeson.Number {} -> OpenApiNumber
                      Aeson.Bool {} -> OpenApiBoolean
                      Aeson.Null -> OpenApiNull
                  }
      BimapCodec _ _ c -> go c
      ObjectOfCodec mname oc -> do
        ss <- goObject oc
        pure $ NamedSchema mname $ combineObjectSchemas ss
      EitherCodec u c1 c2 ->
        let orNull :: forall input output. ValueCodec input output -> StateT (HashMap Text Schema) (Declare (Definitions Schema)) NamedSchema
            orNull c = do
              ns <- go c
              pure $ ns & schema . nullable ?~ True
         in case (c1, c2) of
              (NullCodec, c) -> orNull c
              (c, NullCodec) -> orNull c
              _ -> do
                ns1 <- go c1
                ns2 <- go c2
                combineSchemasOr u ns1 ns2
      CommentCodec t c -> do
        NamedSchema mName s <- go c
        pure $ NamedSchema mName $ addDoc t s
      ReferenceCodec n c -> do
        seenSchemas <- State.get
        case HashMap.lookup n seenSchemas of
          Nothing -> do
            existingDeclaredSchemas <- look

            -- Insert a dummy schema to prevent an infinite loop in recursive data structures
            let dummySchema = mempty
            let seenSchemas' = HashMap.insert n dummySchema seenSchemas

            -- Run in a new isolated Declare monad so that we can get the results and override
            -- the dummy before declaring it in our main Declare monad (Declare does not allow overriding itself)
            let (newDeclaredSchemas, (namedSchema, newSeenSchemas)) = flip runDeclare existingDeclaredSchemas . flip runStateT seenSchemas' $ go c

            -- Override the dummy now we actually know what the result will be
            State.put $ HashMap.insert n (_namedSchemaSchema namedSchema) newSeenSchemas
            declare $ InsOrdHashMap.insert n (_namedSchemaSchema namedSchema) newDeclaredSchemas
            pure $ namedSchema {_namedSchemaName = Just n}
          Just s ->
            -- We've been here before recursively, just reuse the schema we've previously created
            pure $ NamedSchema (Just n) s

    goObject :: ObjectCodec input output -> StateT (HashMap Text Schema) (Declare (Definitions Schema)) [Schema]
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
        let addDefaultToSchema propertySchema = propertySchema {_schemaDefault = Just $ toJSONVia vs defaultValue}
        pure
          [ mempty
              { _schemaProperties = [(key, addDefaultToSchema . addMDoc mDoc . _namedSchemaSchema <$> ref)],
                _schemaType = Just OpenApiObject
              }
          ]
      OptionalKeyWithOmittedDefaultCodec key vs defaultValue mDoc -> goObject (OptionalKeyWithDefaultCodec key vs defaultValue mDoc)
      PureCodec _ -> pure []
      EitherCodec u oc1 oc2 -> do
        s1s <- goObject oc1
        s2s <- goObject oc2
        (: []) . _namedSchemaSchema
          <$> combineSchemasOr
            u
            (NamedSchema Nothing (combineObjectSchemas s1s))
            (NamedSchema Nothing (combineObjectSchemas s2s))
      DiscriminatedUnionCodec pn _ m -> do
        let d =
              Discriminator
                { _discriminatorPropertyName = pn,
                  _discriminatorMapping = InsOrdHashMap.fromHashMap $ fmap fst m
                }
            mkSchema dName (refName, oc) = do
              s <- goObject $ oc *> (requiredFieldWith' pn (literalTextCodec dName) .= const dName)
              declareSpecificSchemaRef (Just refName) $ combineObjectSchemas s
        ss <- HashMap.traverseWithKey mkSchema m
        pure
          [ mempty
              { _schemaDiscriminator = Just d,
                _schemaOneOf = Just $ Foldable.toList ss
              }
          ]
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

    combineSchemasOr :: MonadDeclare (Definitions Schema) m => Union -> NamedSchema -> NamedSchema -> m NamedSchema
    combineSchemasOr u ns1 ns2 = do
      let s1 = _namedSchemaSchema ns1
      let s2 = _namedSchemaSchema ns2
      s1Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns1
      s2Ref <- fmap _namedSchemaSchema <$> declareSpecificNamedSchemaRef ns2
      let orLens :: Lens' Schema (Maybe [Referenced Schema])
          orLens = case u of
            PossiblyJointUnion -> anyOf
            DisjointUnion -> oneOf
      let prototype =
            mempty
              { _schemaAdditionalProperties = case u of
                  PossiblyJointUnion -> Just $ AdditionalPropertiesAllowed True
                  DisjointUnion -> Nothing
              }
      pure $
        NamedSchema Nothing $ case (s1 ^. enum_, s2 ^. enum_) of
          -- If both schemas are enums with the same type then combine their values
          (Just s1enums, Just s2enums)
            | s1 ^. type_ == s2 ^. type_ ->
              prototype
                & enum_ ?~ (s1enums ++ s2enums)
                & type_ .~ s1 ^. type_
          _ ->
            case (s1 ^. orLens, s2 ^. orLens) of
              (Just s1s, Just s2s) -> prototype & orLens ?~ (s1s ++ s2s)
              (Just s1s, Nothing) -> prototype & orLens ?~ (s1s ++ [s2Ref])
              (Nothing, Just s2s) -> prototype & orLens ?~ (s1Ref : s2s)
              (Nothing, Nothing) -> prototype & orLens ?~ [s1Ref, s2Ref]

declareSpecificNamedSchemaRef :: MonadDeclare (Definitions Schema) m => OpenAPI.NamedSchema -> m (Referenced NamedSchema)
declareSpecificNamedSchemaRef namedSchema =
  fmap (NamedSchema (_namedSchemaName namedSchema))
    <$> declareSpecificSchemaRef (_namedSchemaName namedSchema) (_namedSchemaSchema namedSchema)

declareSpecificSchemaRef :: MonadDeclare (Definitions Schema) m => Maybe Text -> OpenAPI.Schema -> m (Referenced Schema)
declareSpecificSchemaRef mName s =
  case mName of
    Nothing -> pure $ Inline s
    Just n -> do
      known <- looks (InsOrdHashMap.member n)
      when (not known) $ declare $ InsOrdHashMap.singleton n s
      pure $ Ref (Reference n)
