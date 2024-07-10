{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Schema where

import Autodocodec
import qualified Autodocodec.Aeson.Compat as Compat
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Aeson ()
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | A JSON Schema
--
-- http://json-schema.org/understanding-json-schema/reference/index.html
--
-- Contrary to a 'Codec', values of this type should be finite.
--
-- NOTE: This schema roundtrips to JSON, but it cannot expres everything that a fully-featured json-schema may be able to express.
data JSONSchema
  = AnySchema
  | NullSchema
  | BoolSchema
  | StringSchema
  | NumberSchema !(Maybe NumberBounds)
  | ArraySchema !JSONSchema
  | MapSchema !JSONSchema
  | -- | This needs to be a list because keys should stay in their original ordering.
    ObjectSchema !ObjectSchema
  | ValueSchema !JSON.Value
  | AnyOfSchema !(NonEmpty JSONSchema)
  | OneOfSchema !(NonEmpty JSONSchema)
  | CommentSchema !Text !JSONSchema
  | RefSchema !Text
  | WithDefSchema !(Map Text JSONSchema) !JSONSchema
  deriving (Show, Eq, Ord, Generic)

instance Validity JSONSchema where
  validate js =
    mconcat
      [ genericValidate js,
        declare "never has two nested comments" $ case js of
          CommentSchema _ (CommentSchema _ _) -> False
          _ -> True,
        case js of
          AnyOfSchema cs -> declare "there are 2 of more choices" $ length cs >= 2
          OneOfSchema cs -> declare "there are 2 of more choices" $ length cs >= 2
          _ -> valid
      ]

instance ToJSON JSONSchema where
  toJSON = JSON.object . go
    where
      go :: JSONSchema -> [JSON.Pair]
      go = \case
        AnySchema -> []
        NullSchema -> ["type" JSON..= ("null" :: Text)]
        BoolSchema -> ["type" JSON..= ("boolean" :: Text)]
        StringSchema -> ["type" JSON..= ("string" :: Text)]
        NumberSchema mBounds ->
          ("type" JSON..= ("number" :: Text)) : case mBounds of
            Nothing -> []
            Just NumberBounds {..} -> ["minimum" JSON..= numberBoundsLower, "maximum" JSON..= numberBoundsUpper]
        ArraySchema s ->
          let itemSchemaVal = go s
           in ["type" JSON..= ("array" :: Text), ("items", JSON.object itemSchemaVal)]
        ValueSchema v -> ["const" JSON..= v]
        MapSchema s ->
          let itemSchemaVal = go s
           in ["type" JSON..= ("object" :: Text), "additionalProperties" JSON..= JSON.object itemSchemaVal]
        ObjectSchema os ->
          case toJSON os of
            JSON.Object o -> Compat.toList o
            _ -> [] -- Should not happen.
        AnyOfSchema jcs ->
          let svals :: [JSON.Value]
              svals = map (JSON.object . go) (NE.toList jcs)
              val :: JSON.Value
              val = (JSON.toJSON :: [JSON.Value] -> JSON.Value) svals
           in [("anyOf", val)]
        OneOfSchema jcs ->
          let svals :: [JSON.Value]
              svals = map (JSON.object . go) (NE.toList jcs)
              val :: JSON.Value
              val = (JSON.toJSON :: [JSON.Value] -> JSON.Value) svals
           in [("oneOf", val)]
        (CommentSchema outerComment (CommentSchema innerComment s)) ->
          go (CommentSchema (outerComment <> "\n" <> innerComment) s)
        CommentSchema comment s -> ("$comment" JSON..= comment) : go s
        RefSchema name -> ["$ref" JSON..= (defsPrefix <> name :: Text)]
        WithDefSchema defs s -> ("$defs" JSON..= defs) : go s

instance FromJSON JSONSchema where
  parseJSON = JSON.withObject "JSONSchema" $ \o -> do
    mt <- o JSON..:? "type"
    mc <- o JSON..:? "$comment"
    let commentFunc = maybe id CommentSchema mc
    mdefs <- o JSON..:? "$defs"
    let defsFunc = maybe id WithDefSchema mdefs
    fmap (commentFunc . defsFunc) $ case mt :: Maybe Text of
      Just "null" -> pure NullSchema
      Just "boolean" -> pure BoolSchema
      Just "string" -> pure StringSchema
      Just "number" -> do
        mLower <- o JSON..:? "minimum"
        mUpper <- o JSON..:? "maximum"
        pure $
          NumberSchema $ case (,) <$> mLower <*> mUpper of
            Nothing -> Nothing
            Just (numberBoundsLower, numberBoundsUpper) -> Just NumberBounds {..}
      Just "array" -> do
        mI <- o JSON..:? "items"
        case mI of
          Nothing -> pure $ ArraySchema AnySchema
          Just is -> pure $ ArraySchema is
      Just "object" -> do
        mAdditional <- o JSON..:? "additionalProperties"
        case mAdditional of
          Nothing -> ObjectSchema <$> parseJSON (JSON.Object o)
          Just additional -> pure $ MapSchema additional
      Nothing -> do
        mAny <- o JSON..:? "anyOf"
        case mAny of
          Just anies -> pure $ AnyOfSchema anies
          Nothing -> do
            mOne <- o JSON..:? "oneOf"
            case mOne of
              Just ones -> pure $ OneOfSchema ones
              Nothing -> do
                let mConst = Compat.lookupKey "const" o
                case mConst of
                  Just constant -> pure $ ValueSchema constant
                  Nothing -> do
                    mRef <- o JSON..:? "$ref"
                    pure $ case mRef of
                      Just ref -> case T.stripPrefix defsPrefix ref of
                        Just name -> RefSchema name
                        Nothing -> AnySchema
                      Nothing -> AnySchema
      t -> fail $ "unknown schema type:" <> show t

data ObjectSchema
  = ObjectKeySchema !Text !KeyRequirement !JSONSchema !(Maybe Text)
  | ObjectAnySchema -- For 'pure'
  | ObjectAnyOfSchema !(NonEmpty ObjectSchema)
  | ObjectOneOfSchema !(NonEmpty ObjectSchema)
  | ObjectAllOfSchema !(NonEmpty ObjectSchema)
  deriving (Show, Eq, Ord, Generic)

instance Validity ObjectSchema

instance FromJSON ObjectSchema where
  parseJSON = JSON.withObject "ObjectSchema" go
    where
      go :: JSON.Object -> JSON.Parser ObjectSchema
      go o = do
        t <- o JSON..: "type"
        guard $ t == ("object" :: Text)
        mAllOf <- o JSON..:? "allOf"
        case mAllOf of
          Just ao -> do
            ne <- parseJSON ao
            ObjectAllOfSchema <$> mapM go ne
          Nothing -> do
            mAnyOf <- o JSON..:? "anyOf"
            case mAnyOf of
              Just anies -> do
                ne <- parseJSON anies
                ObjectAnyOfSchema <$> mapM go ne
              Nothing -> do
                mOneOf <- o JSON..:? "oneOf"
                case mOneOf of
                  Just ones -> do
                    ne <- parseJSON ones
                    ObjectOneOfSchema <$> mapM go ne
                  Nothing -> do
                    props <- o JSON..:? "properties" JSON..!= HM.empty
                    reqs <- o JSON..:? "required" JSON..!= []
                    let keySchemaFor k v = do
                          ks <- parseJSON v
                          let (mDoc, ks') = case ks of
                                CommentSchema doc ks'' -> (Just doc, ks'')
                                _ -> (Nothing, ks)
                          pure $
                            if k `elem` reqs
                              then ObjectKeySchema k Required ks' mDoc
                              else ObjectKeySchema k (Optional Nothing) ks' mDoc
                    keySchemas <- mapM (uncurry keySchemaFor) (HM.toList props)
                    pure $ case NE.nonEmpty keySchemas of
                      Nothing -> ObjectAnySchema
                      Just (el :| []) -> el
                      Just ne -> ObjectAllOfSchema ne

instance ToJSON ObjectSchema where
  toJSON = JSON.object . (("type" JSON..= ("object" :: Text)) :) . go
    where
      go :: ObjectSchema -> [JSON.Pair]
      go = \case
        ObjectAnySchema -> []
        ObjectKeySchema k kr ks mDoc ->
          let (propVal, req) = keySchemaToPieces (k, kr, ks, mDoc)
           in -- TODO deal with the default value somehow.
              concat [["properties" JSON..= JSON.object [Compat.toKey k JSON..= propVal]], ["required" JSON..= [k] | req]]
        ObjectAnyOfSchema ne -> ["anyOf" JSON..= NE.map toJSON ne]
        ObjectOneOfSchema ne -> ["oneOf" JSON..= NE.map toJSON ne]
        ObjectAllOfSchema ne ->
          case mapM parseAndObjectKeySchema (NE.toList ne) of
            Nothing -> ["allOf" JSON..= NE.map toJSON ne]
            Just ne' ->
              let f (hm, l) tup@(k, _, _, _) =
                    let (propVal, req) = keySchemaToPieces tup
                     in (HM.insert k propVal hm, if req then k : l else l)
                  (propValMap, reqs) = foldl' f (HM.empty, []) (concat ne')
               in concat [["properties" JSON..= propValMap], ["required" JSON..= reqs | not $ null reqs]]

      keySchemaToPieces :: (Text, KeyRequirement, JSONSchema, Maybe Text) -> (JSON.Value, Bool)
      keySchemaToPieces (_, kr, ks, mDoc) =
        let propVal = toJSON (maybe id CommentSchema mDoc ks)
         in (propVal, kr == Required)

      parseAndObjectKeySchema :: ObjectSchema -> Maybe [(Text, KeyRequirement, JSONSchema, Maybe Text)]
      parseAndObjectKeySchema = \case
        ObjectKeySchema k kr ks mDoc -> Just [(k, kr, ks, mDoc)]
        ObjectAllOfSchema os -> concat <$> mapM parseAndObjectKeySchema os
        _ -> Nothing

defsPrefix :: Text
defsPrefix = "#/$defs/"

validateAccordingTo :: JSON.Value -> JSONSchema -> Bool
validateAccordingTo val schema = (`evalState` M.empty) $ go val schema
  where
    goObject :: JSON.Object -> ObjectSchema -> State (Map Text JSONSchema) Bool
    goObject obj = \case
      ObjectAnySchema -> pure True
      ObjectKeySchema key kr ks _ -> case Compat.lookupKey (Compat.toKey key) obj of
        Nothing -> case kr of
          Required -> pure False
          Optional _ -> pure True
        Just value' -> go value' ks
      ObjectAllOfSchema ne -> and <$> mapM (goObject obj) ne
      ObjectAnyOfSchema ne -> or <$> mapM (goObject obj) ne
      ObjectOneOfSchema ne -> (== 1) . length . NE.filter id <$> mapM (goObject obj) ne

    go :: JSON.Value -> JSONSchema -> State (Map Text JSONSchema) Bool
    go value = \case
      AnySchema -> pure True
      NullSchema -> pure $ value == JSON.Null
      BoolSchema -> pure $ case value of
        JSON.Bool _ -> True
        _ -> False
      StringSchema -> pure $ case value of
        JSON.String _ -> True
        _ -> False
      NumberSchema mBounds -> pure $ case value of
        JSON.Number s -> case maybe Right checkNumberBounds mBounds s of
          Left _ -> False
          Right _ -> True
        _ -> False
      ArraySchema as -> case value of
        JSON.Array v -> and <$> mapM (`go` as) v
        _ -> pure False
      MapSchema vs -> case value of
        JSON.Object hm -> and <$> mapM (`go` vs) hm
        _ -> pure False
      ObjectSchema os -> case value of
        JSON.Object obj -> goObject obj os
        _ -> pure False
      ValueSchema v -> pure $ v == value
      AnyOfSchema ss -> or <$> mapM (go value) ss
      OneOfSchema ss -> (== 1) . length . NE.filter id <$> mapM (go value) ss
      CommentSchema _ s -> go value s
      RefSchema name -> do
        mSchema <- gets (M.lookup name)
        case mSchema of
          Nothing -> pure False -- Referred to a schema that's not defined, we have no choice but to reject the value.
          Just s -> go value s
      WithDefSchema defs s -> do
        modify (M.union defs)
        go value s

data KeyRequirement
  = Required
  | Optional !(Maybe JSON.Value) -- Default value
  deriving (Show, Eq, Ord, Generic)

instance Validity KeyRequirement

jsonSchemaViaCodec :: forall a. (HasCodec a) => JSONSchema
jsonSchemaViaCodec = jsonSchemaVia (codec @a)

jsonSchemaVia :: ValueCodec input output -> JSONSchema
jsonSchemaVia = (`evalState` S.empty) . go
  where
    go :: ValueCodec input output -> State (Set Text) JSONSchema
    go = \case
      NullCodec -> pure NullSchema
      BoolCodec mname -> pure $ maybe id CommentSchema mname BoolSchema
      StringCodec mname -> pure $ maybe id CommentSchema mname StringSchema
      NumberCodec mname mBounds -> pure $ maybe id CommentSchema mname $ NumberSchema mBounds
      ArrayOfCodec mname c -> do
        s <- go c
        pure $ maybe id CommentSchema mname $ ArraySchema s
      ObjectOfCodec mname oc -> do
        s <- goObject oc
        pure $ maybe id CommentSchema mname $ ObjectSchema s
      HashMapCodec c -> MapSchema <$> go c
      MapCodec c -> MapSchema <$> go c
      ValueCodec -> pure AnySchema
      EqCodec value c -> pure $ ValueSchema (toJSONVia c value)
      EitherCodec u c1 c2 -> do
        s1 <- go c1
        s2 <- go c2
        pure $ case u of
          DisjointUnion -> OneOfSchema (goOneOf (s1 :| [s2]))
          PossiblyJointUnion -> AnyOfSchema (goAnyOf (s1 :| [s2]))
      BimapCodec _ _ c -> go c
      CommentCodec t c -> CommentSchema t <$> go c
      ReferenceCodec name c -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure $ RefSchema name
          else do
            modify (S.insert name)
            s <- go c
            pure $ WithDefSchema (M.singleton name s) (RefSchema name)

    goAnyOf :: NonEmpty JSONSchema -> NonEmpty JSONSchema
    goAnyOf (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goAnyOf ne
      where
        goSingle :: JSONSchema -> NonEmpty JSONSchema
        goSingle = \case
          AnyOfSchema ss -> goAnyOf ss
          s' -> s' :| []
    goOneOf :: NonEmpty JSONSchema -> NonEmpty JSONSchema
    goOneOf (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goOneOf ne
      where
        goSingle :: JSONSchema -> NonEmpty JSONSchema
        goSingle = \case
          OneOfSchema ss -> goOneOf ss
          s' -> s' :| []

    goObject :: ObjectCodec input output -> State (Set Text) ObjectSchema
    goObject = \case
      RequiredKeyCodec k c mdoc -> do
        s <- go c
        pure $ ObjectKeySchema k Required s mdoc
      OptionalKeyCodec k c mdoc -> do
        s <- go c
        pure $ ObjectKeySchema k (Optional Nothing) s mdoc
      OptionalKeyWithDefaultCodec k c mr mdoc -> do
        s <- go c
        pure $ ObjectKeySchema k (Optional (Just (toJSONVia c mr))) s mdoc
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mDoc -> goObject (optionalKeyWithDefaultCodec k c defaultValue mDoc)
      BimapCodec _ _ c -> goObject c
      EitherCodec u oc1 oc2 -> do
        os1 <- goObject oc1
        os2 <- goObject oc2
        pure $ case u of
          DisjointUnion -> ObjectOneOfSchema (goObjectOneOf (os1 :| [os2]))
          PossiblyJointUnion -> ObjectAnyOfSchema (goObjectAnyOf (os1 :| [os2]))
      DiscriminatedUnionCodec pn _ m -> do
        let mkSchema dName (_, oc) =
              goObject $ oc *> (requiredFieldWith' pn (literalTextCodec dName) .= const dName)
        ss <- HM.traverseWithKey mkSchema m
        pure $ case NE.nonEmpty $ toList ss of
          Nothing -> ObjectAnySchema
          Just ss' -> ObjectOneOfSchema $ goObjectOneOf ss'
      PureCodec _ -> pure ObjectAnySchema
      ApCodec oc1 oc2 -> do
        os1 <- goObject oc1
        os2 <- goObject oc2
        pure $ ObjectAllOfSchema (goObjectAllOf (os1 :| [os2]))

    goObjectAnyOf :: NonEmpty ObjectSchema -> NonEmpty ObjectSchema
    goObjectAnyOf (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goObjectAnyOf ne
      where
        goSingle :: ObjectSchema -> NonEmpty ObjectSchema
        goSingle = \case
          ObjectAnyOfSchema ss -> goObjectAnyOf ss
          s' -> s' :| []

    goObjectOneOf :: NonEmpty ObjectSchema -> NonEmpty ObjectSchema
    goObjectOneOf (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goObjectOneOf ne
      where
        goSingle :: ObjectSchema -> NonEmpty ObjectSchema
        goSingle = \case
          ObjectOneOfSchema ss -> goObjectOneOf ss
          s' -> s' :| []

    goObjectAllOf :: NonEmpty ObjectSchema -> NonEmpty ObjectSchema
    goObjectAllOf (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goObjectAllOf ne
      where
        goSingle :: ObjectSchema -> NonEmpty ObjectSchema
        goSingle = \case
          ObjectAllOfSchema ss -> goObjectAllOf ss
          s' -> s' :| []

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c
