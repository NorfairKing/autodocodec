{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.Schema where

import Autodocodec
import Autodocodec.Aeson.Encode
import Control.Applicative
import Control.Monad.State
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
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
  | NumberSchema
  | ArraySchema !JSONSchema
  | -- | This needs to be a list because keys should stay in their original ordering.
    ObjectSchema ![(Text, (KeyRequirement, JSONSchema, Maybe Text))]
  | ValueSchema !JSON.Value
  | ChoiceSchema !(NonEmpty JSONSchema)
  | CommentSchema !Text !JSONSchema
  | RefSchema !Text
  | WithDefSchema !(Map Text JSONSchema) !JSONSchema
  deriving (Show, Eq, Generic)

-- NOTE, this is a recursive schema so we've had to manually write our generators for it.
-- If you add any constructors here, make sure to go add the constructor to the GenValid instance as well.

validateAccordingTo :: JSON.Value -> JSONSchema -> Bool
validateAccordingTo val schema = (`evalState` M.empty) $ go val schema
  where
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
      NumberSchema -> pure $ case value of
        JSON.Number _ -> True
        _ -> False
      ArraySchema as -> case value of
        JSON.Array v -> and <$> mapM (`go` as) v
        _ -> pure False
      ObjectSchema kss -> case value of
        JSON.Object hm ->
          let goKey :: Text -> JSON.Value -> State (Map Text JSONSchema) Bool
              goKey key value' = case lookup key kss of
                Nothing -> pure False
                Just (_, ks, _) -> go value' ks
              goKeySchema :: Text -> (KeyRequirement, JSONSchema, Maybe Text) -> State (Map Text JSONSchema) Bool
              goKeySchema key (kr, ks, _) = case HM.lookup key hm of
                Nothing -> case kr of
                  Required -> pure False
                  Optional _ -> pure True
                Just value' -> go value' ks
              actualKeys = HM.toList hm
           in liftA2 (&&) (and <$> mapM (uncurry goKey) actualKeys) (and <$> mapM (uncurry goKeySchema) kss)
        _ -> pure False
      ValueSchema v -> pure $ v == value
      ChoiceSchema ss -> or <$> mapM (go value) ss
      CommentSchema _ s -> go value s
      RefSchema name -> do
        mSchema <- gets (M.lookup name)
        case mSchema of
          Nothing -> pure False -- Referred to a schema that's not defined, we have no choice but to reject the value.
          Just s -> go value s
      WithDefSchema defs s -> do
        modify (M.union defs)
        go value s

instance Validity JSONSchema where
  validate js =
    mconcat
      [ genericValidate js,
        declare "never has two nested comments" $ case js of
          CommentSchema _ (CommentSchema _ _) -> False
          _ -> True,
        case js of
          ObjectSchema ks ->
            declare "there are no two equal keys in a keys schema" $
              let l = map fst ks
               in nub l == l
          ChoiceSchema cs -> declare "there are 2 of more choices" $ length cs >= 2
          _ -> valid
      ]

data KeyRequirement
  = Required
  | Optional (Maybe JSON.Value) -- Default value
  deriving (Show, Eq, Generic)

instance Validity KeyRequirement

instance ToJSON JSONSchema where
  toJSON = JSON.object . go
    where
      go :: JSONSchema -> [JSON.Pair]
      go = \case
        AnySchema -> []
        NullSchema -> ["type" JSON..= ("null" :: Text)]
        BoolSchema -> ["type" JSON..= ("boolean" :: Text)]
        StringSchema -> ["type" JSON..= ("string" :: Text)]
        NumberSchema -> ["type" JSON..= ("number" :: Text)]
        ArraySchema s ->
          let itemSchemaVal = go s
           in ["type" JSON..= ("array" :: Text), ("items", JSON.object itemSchemaVal)]
        ValueSchema v -> ["const" JSON..= v]
        ObjectSchema os ->
          let combine (ps, rps) (k, (r, s, _)) =
                ( (k, s) : ps,
                  case r of
                    Required -> S.insert k rps
                    Optional _ -> rps
                )

              (props :: [(Text, JSONSchema)], requiredProps) = foldl' combine ([], S.empty) os
              propVals :: HashMap Text JSON.Value
              propVals = HM.map (JSON.object . go) (HM.fromList props)
              propVal :: JSON.Value
              propVal = JSON.toJSON propVals
           in case props of
                [] -> ["type" JSON..= ("object" :: Text)]
                _ ->
                  if S.null requiredProps
                    then
                      [ "type" JSON..= ("object" :: Text),
                        "properties" JSON..= propVal
                      ]
                    else
                      [ "type" JSON..= ("object" :: Text),
                        "properties" JSON..= propVal,
                        "required" JSON..= requiredProps
                      ]
        ChoiceSchema jcs ->
          let svals :: [JSON.Value]
              svals = map (JSON.object . go) (NE.toList jcs)
              val :: JSON.Value
              val = (JSON.toJSON :: [JSON.Value] -> JSON.Value) svals
           in [("anyOf", val)]
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
      Just "number" -> pure NumberSchema
      Just "array" -> do
        mI <- o JSON..:? "items"
        case mI of
          Nothing -> pure $ ArraySchema AnySchema
          Just is -> pure $ ArraySchema is
      Just "object" -> do
        mP <- o JSON..:? "properties"
        case mP of
          Nothing -> pure $ ObjectSchema []
          Just (props :: Map Text JSONSchema) -> do
            requiredProps <- fromMaybe [] <$> o JSON..:? "required"
            let keySchemaFor k s =
                  ( k,
                    ( if k `elem` requiredProps
                        then Required
                        else Optional Nothing, -- TODO
                      s,
                      Nothing -- TODO
                    )
                  )
            pure $ ObjectSchema $ map (uncurry keySchemaFor) $ M.toList props
      Nothing -> do
        mAny <- o JSON..:? "anyOf"
        case mAny of
          Just anies -> pure $ ChoiceSchema anies
          Nothing -> do
            let mConst = HM.lookup "const" o
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

defsPrefix :: Text
defsPrefix = "#/$defs/"

jsonSchemaViaCodec :: forall a. HasCodec a => JSONSchema
jsonSchemaViaCodec = jsonSchemaVia (codec @a)

jsonSchemaVia :: ValueCodec input output -> JSONSchema
jsonSchemaVia = (`evalState` S.empty) . go
  where
    go :: ValueCodec input output -> State (Set Text) JSONSchema
    go = \case
      NullCodec -> pure NullSchema
      BoolCodec mname -> pure $ maybe id CommentSchema mname BoolSchema
      StringCodec mname -> pure $ maybe id CommentSchema mname StringSchema
      NumberCodec mname -> pure $ maybe id CommentSchema mname NumberSchema
      ArrayOfCodec mname c -> do
        s <- go c
        pure $ maybe id CommentSchema mname $ ArraySchema s
      ObjectOfCodec mname oc -> do
        s <- goObject oc
        pure $ maybe id CommentSchema mname $ ObjectSchema s
      ObjectCodec -> pure $ ObjectSchema []
      ValueCodec -> pure AnySchema
      EqCodec value c -> pure $ ValueSchema (toJSONVia c value)
      EitherCodec c1 c2 -> do
        s1 <- go c1
        s2 <- go c2
        pure $ ChoiceSchema (goChoice (s1 :| [s2]))
      MapCodec _ _ c -> go c
      CommentCodec t c -> CommentSchema t <$> go c
      ReferenceCodec name c -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure $ RefSchema name
          else do
            modify (S.insert name)
            s <- go c
            pure $ WithDefSchema (M.singleton name s) (RefSchema name)

    goChoice :: NonEmpty JSONSchema -> NonEmpty JSONSchema
    goChoice (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goChoice ne
      where
        goSingle :: JSONSchema -> NonEmpty JSONSchema
        goSingle = \case
          ChoiceSchema ss -> goChoice ss
          s' -> s' :| []

    goObject :: ObjectCodec input output -> State (Set Text) [(Text, (KeyRequirement, JSONSchema, Maybe Text))]
    goObject = \case
      RequiredKeyCodec k c mdoc -> do
        s <- go c
        pure [(k, (Required, s, mdoc))]
      OptionalKeyCodec k c mdoc -> do
        s <- go c
        pure [(k, (Optional Nothing, s, mdoc))]
      OptionalKeyWithDefaultCodec k c mr mdoc -> do
        s <- go c
        pure [(k, (Optional (Just (toJSONVia c mr)), s, mdoc))]
      MapCodec _ _ c -> goObject c
      PureCodec _ -> pure []
      ApCodec oc1 oc2 -> liftA2 (++) (goObject oc1) (goObject oc2)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c
