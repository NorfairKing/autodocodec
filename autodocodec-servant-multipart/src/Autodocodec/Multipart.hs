{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Multipart where

import Autodocodec
import Servant.Multipart

toMultipartViaCodec :: forall a. HasCodec a => a -> MultipartData Tmp
toMultipartViaCodec = toMultipartVia (codec @a)

toMultipartVia :: ValueCodec a void -> a -> MultipartData Tmp
toMultipartVia c _ = MultipartData [] []

fromMultipartViaCodec :: forall a. HasCodec a => MultipartData Tmp -> Either String a
fromMultipartViaCodec = fromMultipartVia (codec @a)

fromMultipartVia :: ValueCodec void a -> MultipartData Tmp -> Either String a
fromMultipartVia c _ = Left "err"
