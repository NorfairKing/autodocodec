{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
-- Because of the Foldable import, instead of CPP.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- This module is for parsing JSON (or Yaml) values with:
--
-- 1. Nice error messages, and
-- 2. Warnings about unrecognised fields.
--
-- This should be better suited for parsing configuration values than
-- Autodocodec.Aeson.Decode, but it may be slower.
module Autodocodec.Exact
  ( -- * Decoding JSON Values
    parseExactJSONViaCodec,
    parseExactJSONVia,

    -- ** Decoding JSON Objects
    parseExactJSONObjectViaCodec,
    parseExactJSONObjectVia,
    --
    ExactParseError (..),
    prettyExactParseError,
    ExactParseWarning (..),
    prettyExactParseWarning,
    ExactParseContext (..),
    ExactParseContextPiece (..),
    prettyWrapInContext,
    prettyExactParseContextPiece,
  )
where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Class
import Autodocodec.Codec
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Scientific as Scientific
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Text.Show.Pretty

-- | Implement 'JSON.parseExactJSON' via a type's codec.
parseExactJSONViaCodec :: (HasCodec a) => JSON.Value -> Either ExactParseError (a, [ExactParseWarning])
parseExactJSONViaCodec = parseExactJSONVia codec

-- | Implement 'JSON.parseExactJSON' via a given codec.
parseExactJSONVia :: ValueCodec void a -> JSON.Value -> Either ExactParseError (a, [ExactParseWarning])
parseExactJSONVia c v = runExactParser $ goV v c

parseExactJSONObjectViaCodec :: (HasObjectCodec a) => JSON.Object -> Either ExactParseError (a, [ExactParseWarning])
parseExactJSONObjectViaCodec = parseExactJSONObjectVia objectCodec

parseExactJSONObjectVia :: ObjectCodec void a -> JSON.Object -> Either ExactParseError (a, [ExactParseWarning])
parseExactJSONObjectVia c o = runExactParser $ withUnrecognisedKeys o (goO o c)

exactParseKey :: (FromJSONKey key) => Key -> ExactParser key
exactParseKey key =
  withContext (ExactParseContextPieceKeyParser key) $
    case fromJSONKey of
      FromJSONKeyCoerce -> pure $ coerce (Compat.fromKey key)
      FromJSONKeyText f -> pure $ f $ Compat.fromKey key
      FromJSONKeyTextParser f -> case JSON.parseEither f (Compat.fromKey key) of
        Left err -> exactError $ ExactParseErrorKeyParseError key err
        Right k -> pure k
      -- Is this correct?
      -- I don't understand what this option is for.
      FromJSONKeyValue f -> case JSON.parseEither f (JSON.String (Compat.fromKey key)) of
        Left err -> exactError $ ExactParseErrorKeyParseError key err
        Right k -> pure k

-- We use type-annotations here for readability of type information that is
-- gathered to case-matching on GADTs, they aren't strictly necessary.
goV :: JSON.Value -> Codec JSON.Value void a -> ExactParser a
goV value = \case
  NullCodec ->
    case value of
      JSON.Null -> return $ coerce ()
      _ -> exactError $ ExactParseErrorTypeMismatch "null" value
  BoolCodec mname ->
    withNamed mname $
      case value of
        JSON.Bool b -> return $ coerce b
        _ -> exactError $ ExactParseErrorTypeMismatch "a boolean" value
  StringCodec mname ->
    withNamed mname $
      case value of
        JSON.String s -> return $ coerce s
        _ -> exactError $ ExactParseErrorTypeMismatch "a string" value
  IntegerCodec mname bounds ->
    withNamed mname $
      case value of
        JSON.Number unsafeS ->
          let safetyBounds =
                Bounds
                  { boundsLower = Just $ scientific (-1) 1024,
                    boundsUpper = Just $ scientific 1 1024
                  }
           in case checkBounds safetyBounds unsafeS of
                Left err -> exactError $ ExactParseErrorUnsafeNumber safetyBounds unsafeS err
                Right s ->
                  case Scientific.floatingOrInteger s :: Either Double Integer of
                    Left _ -> exactError $ ExactParseErrorTypeMismatch "an integer" value
                    Right i -> case checkBounds bounds i of
                      Left err -> exactError $ ExactParseErrorIntegerOutOfBounds bounds i err
                      Right i' -> pure $ coerce i'
        _ -> exactError $ ExactParseErrorTypeMismatch "an integer" value
  NumberCodec mname bounds ->
    withNamed mname $
      case value of
        JSON.Number s ->
          case checkBounds bounds s of
            Left err -> exactError $ ExactParseErrorNumberOutOfBounds bounds s err
            Right s' -> pure $ coerce s'
        _ -> exactError $ ExactParseErrorTypeMismatch "a number" value
  ArrayOfCodec mname c ->
    withNamed mname $
      case value of
        JSON.Array array ->
          coerce
            <$> Vector.imapM
              ( \ix v ->
                  withContext (ExactParseContextPieceArrayIndex ix) $
                    goV v c
              )
              array
        _ -> exactError $ ExactParseErrorTypeMismatch "an array" value
  ObjectOfCodec mname oc ->
    withNamed mname $
      case value of
        JSON.Object o ->
          withUnrecognisedKeys o $
            goO o oc
        _ -> exactError $ ExactParseErrorTypeMismatch "an object" value
  HashMapCodec c ->
    case value of
      JSON.Object o ->
        coerce $
          withUnrecognisedKeys o $
            -- TODO warn on duplicate keys that may come from the key being
            -- parsed and then erased in HashMap.fromList.
            HashMap.fromList
              <$> mapM
                ( \(key, val) ->
                    (,)
                      <$> ( do
                              recogniseKey key
                              liftValueParser $ exactParseKey key
                          )
                      <*> withContext
                        (ExactParseContextPieceKey key Nothing)
                        (liftValueParser (goV val c))
                )
                (Compat.toList o)
      _ -> exactError $ ExactParseErrorTypeMismatch "an object" value
  MapCodec c ->
    case value of
      JSON.Object o ->
        coerce $
          withUnrecognisedKeys o $
            -- TODO warn on duplicate keys that may come from the key being
            -- parsed and then erased in Map.fromList.
            Map.fromList
              <$> mapM
                ( \(key, val) ->
                    (,)
                      <$> ( do
                              recogniseKey key
                              liftValueParser $ exactParseKey key
                          )
                      <*> withContext
                        (ExactParseContextPieceKey key Nothing)
                        (liftValueParser (goV val c))
                )
                (Compat.toList o)
      _ -> exactError $ ExactParseErrorTypeMismatch "an object" value
  ValueCodec ->
    pure $ coerce value
  EqCodec expected c -> do
    actual <- goV value c
    if expected == actual
      then pure (coerce actual)
      else exactError $ ExactParseErrorExactMatch (ppShow actual) (ppShow expected)
  BimapCodec f _ c -> do
    old <- goV value c
    case f old of
      Left err -> exactError $ ExactParseErrorBimapFailure err
      Right new -> pure new
  EitherCodec u c1 c2 -> do
    ctx <- ask
    let errOrFirst = runExactParserWithContext ctx $ Left <$> goV value c1
        errOrSecond = runExactParserWithContext ctx $ Right <$> goV value c2
    case u of
      PossiblyJointUnion -> coerce $ case errOrFirst of
        Right (l, ws) -> do
          tell ws :: ExactParser ()
          pure l
        Left lErr -> case errOrSecond of
          Right (r, ws) -> do
            tell ws
            pure r
          Left rErr ->
            exactError $ ExactParseErrorPossiblyJointBothFailed lErr rErr
      DisjointUnion -> coerce $
        case (errOrFirst, errOrSecond) of
          (Right (l, ws), Left _) -> do
            tell ws :: ExactParser ()
            pure l
          (Left _, Right (r, ws)) -> do
            tell ws
            pure r
          (Right _, Right _) -> exactError ExactParseErrorDisjointBothSucceeded
          (Left lErr, Left rErr) ->
            exactError $ ExactParseErrorDisjointBothFailed lErr rErr
  CommentCodec comment c ->
    withContext (ExactParseContextPieceComment comment) $
      goV value c
  ReferenceCodec ref c ->
    withContext (ExactParseContextPieceReference ref) $
      goV value c

goO :: JSON.Object -> Codec JSON.Object void a -> ExactObjectParser a
goO value = \case
  BimapCodec f _ c -> do
    old <- goO value c
    case f old of
      Left err -> exactError $ ExactParseErrorBimapFailure err
      Right new -> pure new
  EitherCodec u c1 c2 -> do
    ctx <- ask
    before <- get
    let errOrFirst = runExactObjectParserWithContext ctx before $ Left <$> goO value c1
        errOrSecond = runExactObjectParserWithContext ctx before $ Right <$> goO value c2
    case u of
      PossiblyJointUnion ->
        coerce $
          case errOrFirst of
            Right ((l, s), ws) -> do
              tell ws :: ExactObjectParser ()
              put s
              pure l
            Left lErr -> case errOrSecond of
              Right ((r, s), ws) -> do
                tell ws
                put s
                pure r
              Left rErr ->
                exactError $ ExactParseErrorPossiblyJointBothFailed lErr rErr
      DisjointUnion ->
        coerce $
          case (errOrFirst, errOrSecond) of
            (Right ((l, s), ws), Left _) -> do
              tell ws
              put s
              pure l
            (Left _, Right ((r, s), ws)) -> do
              tell ws :: ExactObjectParser ()
              put s
              pure r
            (Right _, Right _) -> exactError ExactParseErrorDisjointBothSucceeded
            (Left lErr, Left rErr) ->
              exactError $ ExactParseErrorDisjointBothFailed lErr rErr
  DiscriminatedUnionCodec discriminatorName _ hm -> do
    let key = Compat.toKey discriminatorName
    discriminator <- withContext (ExactParseContextPieceKey key Nothing) $ do
      case Compat.lookupKey key (value :: JSON.Object) of
        Nothing -> exactError $ ExactParseErrorMissingDiscriminator key value
        Just discriminatorValue -> do
          recogniseKey key
          case discriminatorValue of
            JSON.String discriminator -> pure discriminator
            _ -> exactError $ ExactParseErrorTypeMismatch "a string" discriminatorValue
    case HashMap.lookup discriminator hm of
      Nothing -> exactError $ ExactParseErrorUnknownDiscriminator discriminator value
      Just (codecName, c) ->
        withContext (ExactParseContextPieceDiscriminator discriminator codecName) $
          goO value c
  RequiredKeyCodec k c mDoc ->
    coerce $ do
      let key = Compat.toKey k
      case Compat.lookupKey key value of
        Nothing -> exactError $ ExactParseErrorMissingRequiredKey key value
        Just v ->
          withContext (ExactParseContextPieceKey key mDoc) $ do
            recogniseKey key
            liftValueParser $
              goV v c
  OptionalKeyCodec k c mDoc ->
    coerce $ do
      let key = Compat.toKey k
      forM (Compat.lookupKey key value) $ \v ->
        withContext (ExactParseContextPieceKey key mDoc) $ do
          recogniseKey key
          liftValueParser $
            goV v c
  OptionalKeyWithDefaultCodec k c defaultValue mDoc ->
    coerce $ do
      let key = Compat.toKey k
      case Compat.lookupKey key value of
        Nothing -> pure defaultValue
        Just v ->
          withContext (ExactParseContextPieceKey key mDoc) $ do
            recogniseKey key
            liftValueParser $
              goV v c
  OptionalKeyWithOmittedDefaultCodec k c defaultValue mDoc ->
    coerce $ do
      let key = Compat.toKey k
      case Compat.lookupKey key value of
        Nothing -> pure defaultValue
        Just v -> do
          recogniseKey key
          withContext (ExactParseContextPieceKey key mDoc) $
            liftValueParser $
              goV v c
  PureCodec a ->
    pure a
  ApCodec ocf oca ->
    goO (value :: JSON.Object) ocf <*> goO (value :: JSON.Object) oca

newtype ExactObjectParser a = ExactObjectParser (StateT (Set Key) ExactParser a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [ExactParseWarning],
      MonadReader ExactParseContext,
      MonadState (Set Key),
      MonadError ExactParseError
    )

withUnrecognisedKeys :: JSON.Object -> ExactObjectParser a -> ExactParser a
withUnrecognisedKeys o (ExactObjectParser func) = do
  (result, leftovers) <- runStateT func (Compat.keysSet o)
  forM_ (NonEmpty.nonEmpty (Set.toList leftovers)) $ \ne ->
    exactWarning (ExactParseWarningUnrecognisedKeys ne)
  pure result

recogniseKey :: Key -> ExactObjectParser ()
recogniseKey = modify . Set.delete

liftValueParser :: ExactParser a -> ExactObjectParser a
liftValueParser func = ExactObjectParser $ lift func

runExactObjectParserWithContext :: ExactParseContext -> Set Key -> ExactObjectParser a -> Either ExactParseError ((a, Set Key), [ExactParseWarning])
runExactObjectParserWithContext ctx set (ExactObjectParser func) = do
  runExactParserWithContext ctx (runStateT func set)

newtype ExactParser a = ExactParser (WriterT [ExactParseWarning] (ReaderT ExactParseContext (Either ExactParseError)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [ExactParseWarning],
      MonadReader ExactParseContext,
      MonadError ExactParseError
    )

runExactParser :: ExactParser a -> Either ExactParseError (a, [ExactParseWarning])
runExactParser = runExactParserWithContext emptyExactParseContext

runExactParserWithContext :: ExactParseContext -> ExactParser a -> Either ExactParseError (a, [ExactParseWarning])
runExactParserWithContext ctx (ExactParser func) = runReaderT (runWriterT func) ctx

newtype ExactParseContext = ExactParseContext {unExactParseContext :: [ExactParseContextPiece]}
  deriving (Show, Eq, Semigroup, Monoid)

emptyExactParseContext :: ExactParseContext
emptyExactParseContext = ExactParseContext []

addContextPiece :: ExactParseContextPiece -> ExactParseContext -> ExactParseContext
addContextPiece piece (ExactParseContext pieces) =
  ExactParseContext (piece : pieces)

withContext ::
  (MonadReader ExactParseContext m) =>
  ExactParseContextPiece ->
  m a ->
  m a
withContext piece =
  local (addContextPiece piece)

withNamed :: Maybe Text -> ExactParser a -> ExactParser a
withNamed = \case
  Nothing -> id
  Just name -> withContext (ExactParseContextPieceNamed name)

prettyWrapInContext :: ExactParseContext -> [String] -> [String]
prettyWrapInContext (ExactParseContext context) message =
  foldl' go message context
  where
    go :: [String] -> ExactParseContextPiece -> [String]
    go ls piece =
      prettyExactParseContextPiece piece ++ ls

data ExactParseContextPiece
  = ExactParseContextPieceKey !Key !(Maybe Text)
  | ExactParseContextPieceArrayIndex !Int
  | ExactParseContextPieceComment !Text
  | ExactParseContextPieceReference !Text
  | ExactParseContextPieceDiscriminator !Text !Text
  | ExactParseContextPieceNamed !Text
  | ExactParseContextPieceKeyParser !Key
  deriving (Show, Eq)

prettyExactParseContextPiece :: ExactParseContextPiece -> [String]
prettyExactParseContextPiece = \case
  ExactParseContextPieceKey key mDoc ->
    case mDoc of
      Nothing -> [unwords ["in the value with key:", show @Key key]]
      Just doc ->
        unwords ["in the value with key:", show @Key key]
          : appendOnLine ["documented:"] (T.unpack doc)
  ExactParseContextPieceArrayIndex ix ->
    [unwords ["in the array at index", show @Int ix, "(so position", show @Int (ix + 1) <> ")"]]
  ExactParseContextPieceComment comment ->
    appendOnLine ["with comment:"] (T.unpack comment)
  ExactParseContextPieceReference ref ->
    appendOnLine ["as part of a schema referred to to as:"] (T.unpack ref)
  ExactParseContextPieceDiscriminator discriminator codecName ->
    [unwords ["with discriminator:", T.unpack discriminator, ", codec: ", T.unpack codecName]]
  ExactParseContextPieceNamed name ->
    appendOnLine ["as part of a schema named:"] (T.unpack name)
  ExactParseContextPieceKeyParser key ->
    appendOnLine ["while trying to parse this key:"] (show @Text (Compat.fromKey key))

exactError ::
  ( MonadReader ExactParseContext m,
    MonadError ExactParseError m
  ) =>
  ExactParseErrorMessage ->
  m a
exactError exactParseErrorMessage = do
  exactParseErrorContext <- ask
  throwError $ ExactParseError {..}

exactWarning :: ExactParseWarningMessage -> ExactParser ()
exactWarning exactParseWarningMessage = do
  exactParseWarningContext <- ask
  tell [ExactParseWarning {..}]

data ExactParseError = ExactParseError
  { exactParseErrorMessage :: ExactParseErrorMessage,
    exactParseErrorContext :: ExactParseContext
  }
  deriving (Show, Eq)

prettyExactParseError :: ExactParseError -> String
prettyExactParseError = unlines . ("Parse error:" :) . exactParseErrorLines

exactParseErrorLines :: ExactParseError -> [String]
exactParseErrorLines ExactParseError {..} =
  prettyWrapInContext exactParseErrorContext (prettyExactParseErrorMessage exactParseErrorMessage)

data ExactParseErrorMessage
  = ExactParseErrorTypeMismatch
      !String
      !JSON.Value
  | ExactParseErrorBimapFailure
      !String
  | ExactParseErrorExactMatch
      !String
      !String
  | ExactParseErrorPossiblyJointBothFailed !ExactParseError !ExactParseError
  | ExactParseErrorDisjointBothSucceeded
  | ExactParseErrorDisjointBothFailed !ExactParseError !ExactParseError
  | ExactParseErrorUnsafeNumber !(Bounds Scientific) !Scientific !String
  | ExactParseErrorNumberOutOfBounds !(Bounds Scientific) !Scientific !String
  | ExactParseErrorIntegerOutOfBounds !(Bounds Integer) !Integer !String
  | ExactParseErrorMissingRequiredKey !Key !JSON.Object
  | ExactParseErrorMissingDiscriminator !Key !JSON.Object
  | ExactParseErrorUnknownDiscriminator !Text !JSON.Object
  | ExactParseErrorKeyParseError !Key !String
  deriving (Show, Eq)

prettyExactParseErrorMessage :: ExactParseErrorMessage -> [String]
prettyExactParseErrorMessage = \case
  ExactParseErrorTypeMismatch expected value ->
    case showValue value of
      [line] -> [unwords ["Type mismatch: expected", expected <> ", but got:", line]]
      ls ->
        unwords ["Type mismatch: expected", expected, ", but got:"]
          : ls
  ExactParseErrorBimapFailure msg ->
    appendOnLine ["failed to parse:"] msg
  ExactParseErrorExactMatch actual expected -> case (lines actual, lines expected) of
    ([lineActual], [lineExpected]) ->
      [unwords ["Expected", lineExpected, "but got", lineActual]]
    ([lineActual], expectedLines) ->
      concat
        [ [unwords ["Expected"]],
          expectedLines,
          [unwords ["but got", lineActual]]
        ]
    (actualLines, [lineExpected]) ->
      concat
        [ [unwords ["Expected", lineExpected, "but got:"]],
          actualLines
        ]
    (actualLines, expectedLines) ->
      concat
        [ [unwords ["Expected:"]],
          expectedLines,
          [unwords ["but got:"]],
          actualLines
        ]
  ExactParseErrorPossiblyJointBothFailed l r ->
    concat
      [ ["Disjoint union: both branches failed:"],
        indent $ exactParseErrorLines l,
        ["and"],
        indent $ exactParseErrorLines r
      ]
  ExactParseErrorDisjointBothSucceeded ->
    ["Disjoint union: both branches succeeded, this is invalid."]
  ExactParseErrorDisjointBothFailed l r ->
    concat
      [ ["Disjoint union: both branches failed:"],
        indent $ exactParseErrorLines l,
        ["and"],
        indent $ exactParseErrorLines r
      ]
  ExactParseErrorUnsafeNumber _ s err ->
    [unwords ["Rejecting number that would cause excessive memory usage:", show @Scientific s], err]
  ExactParseErrorNumberOutOfBounds _ s err ->
    [unwords ["Number is out of bounds:", show @Scientific s], err]
  ExactParseErrorIntegerOutOfBounds _ i err ->
    [unwords ["Integer is out of bounds:", show @Integer i], err]
  ExactParseErrorMissingRequiredKey key o ->
    appendOnLine' ["Missing required key:", show @Key key ++ ", in object:"] (showObject o)
  ExactParseErrorMissingDiscriminator key o ->
    appendOnLine' ["Missing discriminator:", show @Key key ++ ", in object:"] (showObject o)
  ExactParseErrorUnknownDiscriminator discriminator o ->
    appendOnLine' ["Unknown discriminator value:", show @Text discriminator ++ ", in object:"] (showObject o)
  ExactParseErrorKeyParseError key err ->
    appendOnLine ["Error parsing key:", show @Key key ++ ":"] err
  where
    showValue :: JSON.Value -> [String]
    showValue = lines . T.unpack . TE.decodeUtf8 . LB.toStrict . JSON.encodePretty
    showObject :: JSON.Object -> [String]
    showObject = showValue . JSON.Object

    indent :: [String] -> [String]
    indent = map ("  " ++)

appendOnLine :: [String] -> String -> [String]
appendOnLine ws str = appendOnLine' ws (lines str)

appendOnLine' :: [String] -> [String] -> [String]
appendOnLine' ws ls = case ls of
  [] -> [unwords ws]
  [line] -> [unwords $ ws ++ [line]]
  _ -> unwords ws : ls

data ExactParseWarning = ExactParseWarning
  { exactParseWarningContext :: ExactParseContext,
    exactParseWarningMessage :: ExactParseWarningMessage
  }
  deriving (Show, Eq)

prettyExactParseWarning :: ExactParseWarning -> String
prettyExactParseWarning = unlines . ("Parse warning:" :) . exactParseWarningLines

exactParseWarningLines :: ExactParseWarning -> [String]
exactParseWarningLines ExactParseWarning {..} =
  prettyWrapInContext exactParseWarningContext $
    prettyExactParseWarningMessage exactParseWarningMessage

data ExactParseWarningMessage = ExactParseWarningUnrecognisedKeys !(NonEmpty Key)
  deriving (Show, Eq)

prettyExactParseWarningMessage :: ExactParseWarningMessage -> [String]
prettyExactParseWarningMessage = \case
  ExactParseWarningUnrecognisedKeys keys -> case keys of
    key :| [] -> [unwords ["Unrecognised key:", show @Key key]]
    _ -> [unwords ["Unrecognised keys:", intercalate ", " (map (show @Key) (NonEmpty.toList keys))]]
