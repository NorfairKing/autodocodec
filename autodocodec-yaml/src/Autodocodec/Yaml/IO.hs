{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.IO where

import Autodocodec
import Autodocodec.Yaml.Document
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Yaml as Yaml
import Path
import Path.IO
import System.Exit

-- | Helper function to read a yaml file for a type in 'HasCodec'
readYamlConfigFile :: HasCodec a => Path r File -> IO (Maybe a)
readYamlConfigFile p = readFirstYamlConfigFile [p]

-- | Helper function to read the first in a list of yaml files for a type is 'HasCodec'
readFirstYamlConfigFile :: forall a r. HasCodec a => [Path r File] -> IO (Maybe a)
readFirstYamlConfigFile files = go files
  where
    go :: [Path r File] -> IO (Maybe a)
    go =
      \case
        [] -> pure Nothing
        (p : ps) -> do
          mc <- forgivingAbsence $ SB.readFile $ toFilePath p
          case mc of
            Nothing -> go ps
            Just contents ->
              case Yaml.decodeEither' contents of
                Left err -> do
                  let failedMsgs =
                        [ "Failed to parse yaml file",
                          toFilePath p,
                          "with error:",
                          Yaml.prettyPrintParseException err
                        ]
                      triedFilesMsgs = case files of
                        [] -> []
                        [f] -> ["While parsing file: " <> toFilePath f]
                        fs -> "While parsing files:" : map (("* " <>) . toFilePath) fs
                      referenceMsgs =
                        [ "Reference: ",
                          T.unpack $ TE.decodeUtf8With TE.lenientDecode (renderColouredSchemaViaCodec @a)
                        ]
                  die $
                    unlines $
                      concat
                        [ failedMsgs,
                          triedFilesMsgs,
                          referenceMsgs
                        ]
                Right (Autodocodec conf) -> pure $ Just conf
