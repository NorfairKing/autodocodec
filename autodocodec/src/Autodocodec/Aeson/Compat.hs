{-# LANGUAGE CPP #-}

module Autodocodec.Aeson.Compat where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as HM
#endif
import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)
toKey :: Text -> Key
toKey = K.fromText
#else
toKey :: Text -> Text
toKey = id
#endif

#if MIN_VERSION_aeson(2,0,0)
fromKey :: Key -> Text
fromKey = K.toText
#else
fromKey :: Text -> Text
fromKey = id
#endif

#if MIN_VERSION_aeson(2,0,0)
lookupKey :: Key -> KM.KeyMap v -> Maybe v
lookupKey = KM.lookup
#else
lookupKey :: Text -> HM.HashMap Text v -> Maybe v
lookupKey = HM.lookup
#endif

#if MIN_VERSION_aeson(2,0,0)
fromList :: [(Key, v)] -> KM.KeyMap v
fromList = KM.fromList
#else
fromList :: [(Text, v)] -> HM.HashMap Text v
fromList = HM.fromList
#endif
