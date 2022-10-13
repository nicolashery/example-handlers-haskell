module App.Req (isStatusCodeException') where

import Data.ByteString (ByteString)
import Network.HTTP.Client qualified as L
import Network.HTTP.Req (HttpException (VanillaHttpException))

isStatusCodeException' :: HttpException -> Maybe (L.Response (), ByteString)
isStatusCodeException'
  ( VanillaHttpException
      ( L.HttpExceptionRequest
          _
          (L.StatusCodeException r b)
        )
    ) = Just (r, b)
isStatusCodeException' _ = Nothing
