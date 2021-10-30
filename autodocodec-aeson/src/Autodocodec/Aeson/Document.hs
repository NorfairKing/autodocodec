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
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
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
  | -- | This needs to be a list because keys should stay in their original ordering.
    ObjectSchema ![(Text, (KeyRequirement, JSONSchema))]
  | ValueSchema !JSON.Value
  | ChoiceSchema !(NonEmpty JSONSchema)
  | CommentSchema !Text !JSONSchema
  | ReferenceSchema !Text !JSONSchema
  deriving (Eq, Generic)

instance Show JSONSchema where
  show = showJSONSchemaABit

showJSONSchemaABit :: JSONSchema -> String
showJSONSchemaABit = ($ "") . (`evalState` S.empty) . go 0
  where
    go :: Int -> JSONSchema -> State (Set Text) ShowS
    go d = \case
      AnySchema -> pure $ showString "AnySchema"
      NullSchema -> pure $ showString "NullSchema"
      BoolSchema -> pure $ showString "BoolSchema"
      StringSchema -> pure $ showString "StringSchema"
      NumberSchema -> pure $ showString "NumberSchema"
      ArraySchema c -> (\s -> showParen (d > 10) $ showString "ArraySchema " . s) <$> go 11 c
      ObjectSchema kss -> do
        fs <- forM kss $ \(k, (kr, ks)) -> do
          let f1 = showsPrec d k
          let f2 = showsPrec d kr
          f3 <- go d ks
          pure $ f1 . showString " " . f2 . showString " " . f3
        let s = appEndo $ mconcat $ map Endo fs
        pure $ showParen (d > 10) $ showString "ObjectSchema " . s
      ValueSchema v -> pure $ showString "ValueSchema" . showsPrec d v
      ChoiceSchema jcs -> do
        fs <- mapM (go d) (NE.toList jcs)
        let s = appEndo $ mconcat $ map Endo fs
        pure $ showParen (d > 10) $ showString "ChoiceSchema " . s
      CommentSchema comment c -> (\s -> showParen (d > 10) $ showString "CommentSchema " . showsPrec d comment . showString " " . s) <$> go 11 c
      ReferenceSchema name c -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure $ showParen (d > 10) $ showString "ReferenceSchema " . showsPrec d name
          else do
            modify (S.insert name)
            s <- go d c
            pure $ showParen (d > 10) $ showString "ReferenceSchema " . showsPrec d name . showString " " . s

validateAccordingTo :: JSON.Value -> JSONSchema -> Bool
validateAccordingTo = go
  where
    go :: JSON.Value -> JSONSchema -> Bool
    go value = \case
      AnySchema -> True
      NullSchema -> value == JSON.Null
      BoolSchema -> case value of
        JSON.Bool _ -> True
        _ -> False
      StringSchema -> case value of
        JSON.String _ -> True
        _ -> False
      NumberSchema -> case value of
        JSON.Number _ -> True
        _ -> False
      ArraySchema as -> case value of
        JSON.Array v -> all (`validateAccordingTo` as) v
        _ -> False
      ObjectSchema kss -> case value of
        JSON.Object hm ->
          let goKey :: Text -> JSON.Value -> Bool
              goKey key value' = case lookup key kss of
                Nothing -> False
                Just (_, ks) -> go value' ks
              goKeySchema :: Text -> (KeyRequirement, JSONSchema) -> Bool
              goKeySchema key (kr, ks) = case HM.lookup key hm of
                Nothing -> kr == Optional
                Just value' -> go value' ks
              actualKeys = HM.toList hm
           in all (uncurry goKey) actualKeys && all (uncurry goKeySchema) kss
        _ -> False
      ValueSchema v -> v == value
      ChoiceSchema ss -> any (go value) ss
      CommentSchema _ s -> go value s
      ReferenceSchema _ s -> go value s

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

data KeyRequirement = Required | Optional
  deriving (Show, Eq, Generic)

instance Validity KeyRequirement

instance ToJSON JSONSchema where
  toJSON = uncurry objectWithDefs . (`runState` M.empty) . go
    where
      objectWithDefs :: [JSON.Pair] -> Map Text JSON.Value -> JSON.Value
      objectWithDefs pairs defs =
        JSON.object $
          ( if null defs
              then id
              else (("$defs" JSON..= defs) :)
          )
            pairs
      go :: JSONSchema -> State (Map Text JSON.Value) [JSON.Pair]
      go = \case
        AnySchema -> pure []
        NullSchema -> pure ["type" JSON..= ("null" :: Text)]
        BoolSchema -> pure ["type" JSON..= ("boolean" :: Text)]
        StringSchema -> pure ["type" JSON..= ("string" :: Text)]
        NumberSchema -> pure ["type" JSON..= ("number" :: Text)]
        ArraySchema s -> do
          itemSchemaVal <- go s
          pure ["type" JSON..= ("array" :: Text), ("items", JSON.object itemSchemaVal)]
        ValueSchema v -> pure ["const" JSON..= v]
        ObjectSchema os -> do
          let combine (ps, rps) (k, (r, s)) =
                ( (k, s) : ps,
                  case r of
                    Required -> S.insert k rps
                    Optional -> rps
                )
              (props, requiredProps) = foldl' combine ([], S.empty) os
          propVals <- mapM (fmap JSON.object . go) $ HM.fromList props
          let propVal :: JSON.Value
              propVal = (JSON.toJSON :: HashMap Text JSON.Value -> JSON.Value) propVals
          pure $ case props of
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
        ChoiceSchema jcs -> do
          svals <- forM (NE.toList jcs) $ \js -> do
            pairs <- go js
            pure $ JSON.object pairs
          let val :: JSON.Value
              val = (JSON.toJSON :: [JSON.Value] -> JSON.Value) svals
          pure [("anyOf", val)]
        CommentSchema comment s -> (("$comment" JSON..= comment) :) <$> go s
        ReferenceSchema name s -> do
          alreadySeen <- gets (M.member name)
          when (not alreadySeen) $ do
            modify (M.insert name JSON.Null) -- TODO Dummy value
            val <- go s
            modify (M.insert name (JSON.object val))
          -- Here we don't recurse, on purpose.
          pure ["$ref" JSON..= ("#/$defs/" <> name :: Text)]

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
          Nothing -> pure $ ObjectSchema []
          Just (props :: Map Text JSONSchema) -> do
            requiredProps <- fromMaybe [] <$> o JSON..:? "required"
            let keySchemaFor k s =
                  ( k,
                    ( if k `elem` requiredProps
                        then Required
                        else Optional,
                      s
                    )
                  )
            pure $ ObjectSchema $ map (uncurry keySchemaFor) $ M.toList props
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
      ValueCodec -> AnySchema
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
      ReferenceCodec t c -> ReferenceSchema t (go c)

    goChoice :: NonEmpty JSONSchema -> NonEmpty JSONSchema
    goChoice (s :| rest) = case NE.nonEmpty rest of
      Nothing -> goSingle s
      Just ne -> goSingle s <> goChoice ne
      where
        goSingle :: JSONSchema -> NonEmpty JSONSchema
        goSingle = \case
          ChoiceSchema ss -> goChoice ss
          s' -> s' :| []

    goObject :: ObjectCodec input output -> [(Text, (KeyRequirement, JSONSchema))]
    goObject = \case
      RequiredKeyCodec k c -> [(k, (Required, go c))]
      OptionalKeyCodec k c -> [(k, (Optional, go c))]
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> []
      ApObjectCodec oc1 oc2 -> goObject oc1 ++ goObject oc2
