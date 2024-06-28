{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Nix
  ( -- * Producing a Nixos module type
    nixTypeViaCodec,
    nixTypeVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Nix,
  )
where

import Autodocodec
import Data.Text (Text)

-- | Produce a Nixos module type via a type's 'codec'.
nixTypeViaCodec :: forall a. (HasCodec a) => Text
nixTypeViaCodec = nixTypeVia (codec @a)

-- | Parse a Yaml 'ByteString' using a type's 'codec'.
nixTypeVia :: ValueCodec input output -> Text
nixTypeVia = undefined
