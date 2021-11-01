module Autodocodec.DerivingVia where

-- | 'Autodocodec' is a wrapper to provide codec-based deriving strategies.
--
-- === Example usage
--
-- > data Via = Via {viaOne :: !Text, viaTwo :: !Text}
-- >   deriving stock (Show, Eq, Generic)
-- >   deriving (FromJSON, ToJSON) via (Autodocodec Via)
-- >
-- > instance HasCodec Via where
-- >   codec =
-- >     object "Via" $
-- >       Via
-- >         <$> requiredField "one" "first field" .= viaOne
-- >         <*> requiredField "two" "second field" .= viaTwo
newtype Autodocodec a = Autodocodec {unAutodocodec :: a}
