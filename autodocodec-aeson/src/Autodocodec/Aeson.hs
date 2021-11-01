{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Autodocodec.Aeson
  ( -- * To makes sure we definitely export everything.
    module Autodocodec.Aeson.Decode,
    module Autodocodec.Aeson.DerivingVia,
    module Autodocodec.Aeson.Document,
    module Autodocodec.Aeson.Encode,
  )
where

import Autodocodec.Aeson.Decode
import Autodocodec.Aeson.DerivingVia ()
import Autodocodec.Aeson.Document
import Autodocodec.Aeson.Encode
