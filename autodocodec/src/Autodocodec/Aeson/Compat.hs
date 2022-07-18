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
insert :: Key -> v -> KM.KeyMap v -> KM.KeyMap v
insert = KM.insert
#else
insert :: Text -> v -> HM.HashMap Text v -> HM.HashMap Text v
insert = HM.insert
#endif

#if MIN_VERSION_aeson(2,0,0)
fromList :: [(Key, v)] -> KM.KeyMap v
fromList = KM.fromList
#else
fromList :: [(Text, v)] -> HM.HashMap Text v
fromList = HM.fromList
#endif

#if MIN_VERSION_aeson(2,0,0)
toList ::  KM.KeyMap v -> [(Key, v)]
toList = KM.toList
#else
toList ::HM.HashMap Text v -> [(Text, v)]
toList = HM.toList
#endif

#if MIN_VERSION_aeson(2,0,0)
map ::  (v1 -> v2) -> KM.KeyMap v1 -> KM.KeyMap v2
map = KM.map
#else
map ::  (v1 -> v2) -> HM.HashMap Text v1 -> HM.HashMap Text v2
map = HM.map
#endif
