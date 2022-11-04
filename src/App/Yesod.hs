{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Yesod
  ( main,
    -- Suppress warning for unused top-level bindings generated by TH
    resourcesApp,
    Widget,
  )
where

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
import App.Config
  ( Config
      ( configBookingDelay,
        configBookingUrl,
        configDatabaseUrl,
        configPaymentDelay,
        configPaymentUrl,
        configPurchaseDelay
      ),
    configInit,
  )
import App.Db (HasDbPool (getDbPool), dbInit)
import App.Json (defaultToJSON)
import Blammo.Logging
  ( HasLogger (loggerL),
    Logger,
    Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    logWarn,
    runLoggerLoggingT,
    (.=),
  )
import Blammo.Logging.Simple (newLoggerEnv)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON))
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Lens.Micro (lens)
import Network.HTTP.Req
  ( HttpConfig,
    MonadHttp (getHttpConfig, handleHttpException),
    defaultHttpConfig,
  )
import Network.HTTP.Types (Status, status404, status409, status500)
import Network.Wai.Handler.Warp (run)
import UnliftIO (throwIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catch)
import Yesod.Core
  ( MonadHandler,
    PathPiece,
    Yesod (messageLoggerSource),
    getsYesod,
    mkYesod,
    parseRoutes,
    renderRoute,
    sendResponseStatus,
    toWaiAppPlain,
  )
import Yesod.Core.Types (HandlerData (handlerEnv), JSONResponse (JSONResponse), RunHandlerEnv (rheSite))

data App = App
  { appConfig :: Config,
    appLogger :: Logger,
    appHttpConfig :: HttpConfig,
    appDbPool :: Pool Connection
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasCartConfig App where
  getBookingUrl = configBookingUrl . appConfig
  getBookingDelay = configBookingDelay . appConfig
  getPaymentUrl = configPaymentUrl . appConfig
  getPaymentDelay = configPaymentDelay . appConfig

instance HasCartConfig (HandlerData App App) where
  getBookingUrl = getBookingUrl . rheSite . handlerEnv
  getBookingDelay = getBookingDelay . rheSite . handlerEnv
  getPaymentUrl = getPaymentUrl . rheSite . handlerEnv
  getPaymentDelay = getPaymentDelay . rheSite . handlerEnv

instance HasDbPool App where
  getDbPool = appDbPool

instance HasDbPool (HandlerData App App) where
  getDbPool = getDbPool . rheSite . handlerEnv

appInit :: IO App
appInit = do
  config <- configInit
  logger <- newLoggerEnv
  dbPool <- dbInit $ configDatabaseUrl config
  let app =
        App
          { appConfig = config,
            appLogger = logger,
            appHttpConfig = defaultHttpConfig,
            appDbPool = dbPool
          }
  pure app

mkYesod
  "App"
  [parseRoutes|
  /cart/#CartId/purchase CartPurchaseR POST
|]

deriving instance PathPiece CartId

instance MonadHttp Handler where
  handleHttpException = throwIO
  getHttpConfig = getsYesod appHttpConfig

instance Yesod App where
  messageLoggerSource app _logger loc source level msg =
    runLoggerLoggingT app $ monadLoggerLog loc source level msg

sendStatusText :: (MonadHandler m) => Status -> Text -> m a
sendStatusText = sendResponseStatus

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
      sendStatusText status404 "Cart does not exist"
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      sendStatusText status409 "Cart already purchased"
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      sendStatusText status409 "Cart locked"
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
            sendStatusText status500 ("Cart purchase failed: " <> msg)
          Right (bookingId, paymentId) -> do
            purchaseDelay <- getsYesod (configPurchaseDelay . appConfig)
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
  app <- appInit
  waiApp <- toWaiAppPlain app
  let port = 3000
  run port waiApp
