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
toAesonKey :: Text -> Key
toAesonKey = K.fromText

fromAesonKey :: Key -> Text 
fromAesonKey = K.toText

lookupKey :: Key -> KM.KeyMap v -> Maybe v
lookupKey = KM.lookup

toList :: KM.KeyMap v -> [(Key, v)]
toList m =
  KM.toList m

map :: (a -> b) -> KM.KeyMap a -> KM.KeyMap b 
map = KM.map

#else 
toAesonKey :: Text -> Text 
toAesonKey = id

fromAesonKey :: Text -> Text 
fromAesonKey = id

lookupKey :: Text -> HM.HashMap k v -> Maybe v
lookupKey = HM.lookup

toList :: HM.HashMap k v -> [(k, v)]
toList = HM.toList

map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map = HM.map
#endif
