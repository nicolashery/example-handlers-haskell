{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Yesod
  ( main,
    -- Suppress warning for unused top-level bindings generated by TH
    resourcesAppEnv,
    Widget,
  )
where

import App.AppEnv (AppEnv (appEnvConfig, appEnvHttpConfig), appEnvInit)
import App.Cart
  ( BookingId,
    CartException (CartException),
    CartId (CartId),
    CartStatus (CartStatusLocked, CartStatusOpen, CartStatusPurchased),
    HasCartConfig
      ( getBookingDelay,
        getBookingUrl,
        getPaymentDelay,
        getPaymentUrl
      ),
    PaymentId,
    getCartStatus,
    markCartAsPurchased,
    processBooking,
    processPayment,
    withCart,
  )
import App.Config (Config (configPurchaseDelay))
import App.Db (HasDbPool (getDbPool))
import App.Json (defaultToJSON)
import Blammo.Logging
  ( Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    logWarn,
    runLoggerLoggingT,
    (.=),
  )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON), object)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( MonadHttp (getHttpConfig, handleHttpException),
  )
import Network.HTTP.Types (Status, status404, status409, status500)
import Network.Wai.Handler.Warp (run)
import UnliftIO (throwIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catch)
import Yesod.Core
  ( ErrorResponse (InternalError, NotFound),
    MonadHandler,
    PathPiece,
    ToTypedContent (toTypedContent),
    Yesod (errorHandler, makeSessionBackend, messageLoggerSource),
    defaultErrorHandler,
    getsYesod,
    mkYesod,
    parseRoutes,
    renderRoute,
    sendStatusJSON,
    toWaiAppPlain,
  )
import Yesod.Core.Types
  ( HandlerData (handlerEnv),
    JSONResponse (JSONResponse),
    RunHandlerEnv (rheSite),
  )

instance HasCartConfig (HandlerData AppEnv AppEnv) where
  getBookingUrl = getBookingUrl . rheSite . handlerEnv
  getBookingDelay = getBookingDelay . rheSite . handlerEnv
  getPaymentUrl = getPaymentUrl . rheSite . handlerEnv
  getPaymentDelay = getPaymentDelay . rheSite . handlerEnv

instance HasDbPool (HandlerData AppEnv AppEnv) where
  getDbPool = getDbPool . rheSite . handlerEnv

mkYesod
  "AppEnv"
  [parseRoutes|
  /cart/#CartId/purchase CartPurchaseR POST
|]

deriving instance PathPiece CartId

instance MonadHttp Handler where
  handleHttpException = throwIO
  getHttpConfig = getsYesod appEnvHttpConfig

instance Yesod AppEnv where
  messageLoggerSource app _logger loc source level msg =
    runLoggerLoggingT app $ monadLoggerLog loc source level msg

  makeSessionBackend _ = return Nothing

  errorHandler NotFound = do
    let msg :: Text
        msg = "Path not found"
    pure $ toTypedContent $ object ["error" .= msg]
  errorHandler (InternalError msg) = do
    pure $ toTypedContent $ object ["error" .= msg]
  errorHandler e = defaultErrorHandler e

sendStatusError :: (MonadHandler m) => Status -> Text -> m a
sendStatusError s msg = sendStatusJSON s $ object ["error" .= msg]

data CartPurchaseResponse = CartPurchaseResponse
  { cartPurchaseResponseCartId :: CartId,
    cartPurchaseResponsePurchaseDelay :: Int,
    cartPurchaseResponseBookingId :: BookingId,
    cartPurchaseResponsePaymentId :: PaymentId
  }
  deriving (Generic)

instance ToJSON CartPurchaseResponse where
  toJSON = defaultToJSON "cartPurchaseResponse"

postCartPurchaseR :: CartId -> Handler (JSONResponse CartPurchaseResponse)
postCartPurchaseR cartId = do
  cartStatusMaybe <- getCartStatus cartId
  case cartStatusMaybe of
    Nothing -> do
      logWarn $ "Cart does not exist" :# ["cart_id" .= cartId]
      sendStatusError status404 "Cart does not exist"
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      sendStatusError status409 "Cart already purchased"
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      sendStatusError status409 "Cart locked"
    Just CartStatusOpen -> do
      withCart cartId $ do
        logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
        let action :: Handler (Either Text (BookingId, PaymentId))
            action = Right <$> concurrently (processBooking cartId) (processPayment cartId)
            handleError :: CartException -> Handler (Either Text (BookingId, PaymentId))
            handleError (CartException msg) = pure $ Left msg
        result <- catch action handleError
        case result of
          Left msg -> do
            logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
            sendStatusError status500 ("Cart purchase failed: " <> msg)
          Right (bookingId, paymentId) -> do
            purchaseDelay <- getsYesod (configPurchaseDelay . appEnvConfig)
            liftIO $ threadDelay (100 * 1000)
            markCartAsPurchased cartId
            logInfo $ "Cart purchase successful" :# ["cart_id" .= cartId]
            pure $
              JSONResponse $
                CartPurchaseResponse
                  { cartPurchaseResponseCartId = cartId,
                    cartPurchaseResponsePurchaseDelay = purchaseDelay,
                    cartPurchaseResponseBookingId = bookingId,
                    cartPurchaseResponsePaymentId = paymentId
                  }

main :: IO ()
main = do
  app <- appEnvInit
  waiApp <- toWaiAppPlain app
  let port = 3000
  run port waiApp
