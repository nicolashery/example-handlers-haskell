{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Scotty
  ( main,
  )
where

import App.AppEnv (AppEnv (appEnvConfig, appEnvHttpConfig), appEnvInit)
import App.Cart
  ( BookingId,
    CartException (CartException),
    CartId (CartId),
    CartStatus (CartStatusLocked, CartStatusOpen, CartStatusPurchased),
    PaymentId,
    getCartStatus,
    markCartAsPurchased,
    processBooking,
    processPayment,
    withCart,
  )
import App.Config (Config (configPurchaseDelay))
import App.Json (defaultToJSON)
import Blammo.Logging
  ( LoggingT,
    Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    logWarn,
    runLoggerLoggingT,
    (.=),
  )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON (toJSON), object)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( MonadHttp (getHttpConfig, handleHttpException),
  )
import Network.HTTP.Types (Status, status404, status409, status500)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catch)
import Web.Scotty.Trans
  ( ActionT,
    ScottyError (showError, stringError),
    ScottyT,
    defaultHandler,
    json,
    notFound,
    param,
    post,
    raise,
    scottyT,
    status,
  )

newtype App a = App
  { unApp :: ReaderT AppEnv (LoggingT IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO)

instance MonadLogger App where
  monadLoggerLog loc logSource logLevel msg =
    App $ lift $ monadLoggerLog loc logSource logLevel msg

instance (ScottyError e, MonadLogger m) => MonadLogger (ActionT e m) where
  monadLoggerLog loc logSource logLevel msg =
    lift $ monadLoggerLog loc logSource logLevel msg

instance MonadHttp App where
  handleHttpException = throwIO
  getHttpConfig = asks appEnvHttpConfig

runApp :: App a -> IO a
runApp m = do
  app <- appEnvInit
  runLoggerLoggingT app $ runReaderT (unApp m) app

data CartPurchaseResponse = CartPurchaseResponse
  { cartPurchaseResponseCartId :: CartId,
    cartPurchaseResponsePurchaseDelay :: Int,
    cartPurchaseResponseBookingId :: BookingId,
    cartPurchaseResponsePaymentId :: PaymentId
  }
  deriving (Generic)

instance ToJSON CartPurchaseResponse where
  toJSON = defaultToJSON "cartPurchaseResponse"

postCartPurchaseHandler :: ActionT AppError App ()
postCartPurchaseHandler = do
  cartId <- CartId <$> param "cartId"
  cartStatusMaybe <- lift $ getCartStatus cartId
  case cartStatusMaybe of
    Nothing -> do
      logWarn $ "Cart does not exist" :# ["cart_id" .= cartId]
      raiseAppError status404 "Cart does not exist"
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      raiseAppError status409 "Cart already purchased"
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      raiseAppError status409 "Cart locked"
    Just CartStatusOpen -> do
      logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
      let purchase :: App CartPurchaseResponse
          purchase = do
            (bookingId, paymentId) <- concurrently (processBooking cartId) (processPayment cartId)
            purchaseDelay <- asks (configPurchaseDelay . appEnvConfig)
            liftIO $ threadDelay purchaseDelay
            markCartAsPurchased cartId
            pure $
              CartPurchaseResponse
                { cartPurchaseResponseCartId = cartId,
                  cartPurchaseResponsePurchaseDelay = purchaseDelay,
                  cartPurchaseResponseBookingId = bookingId,
                  cartPurchaseResponsePaymentId = paymentId
                }

          action :: App (Either Text CartPurchaseResponse)
          action = Right <$> withCart cartId purchase

          handleError :: CartException -> App (Either Text CartPurchaseResponse)
          handleError (CartException msg) = pure $ Left msg

      result <- lift $ catch action handleError
      case result of
        Left msg -> do
          logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
          raiseAppError status500 ("Cart purchase failed: " <> msg)
        Right response -> do
          logInfo $ "Cart purchase successful" :# ["cart_id" .= cartId]
          json response

data AppError = AppError
  { appErrorStatus :: Status,
    appErrorMessage :: Text
  }

instance ScottyError AppError where
  stringError v = AppError status500 (cs v)
  showError e = cs $ appErrorMessage e

raiseAppError :: (Monad m) => Status -> Text -> ActionT AppError m a
raiseAppError s t =
  raise $
    AppError
      { appErrorStatus = s,
        appErrorMessage = t
      }

handleException :: (Monad m) => AppError -> ActionT AppError m ()
handleException e = do
  status $ appErrorStatus e
  json $ object ["error" .= appErrorMessage e]

handleNotFound :: (Monad m) => ActionT AppError m ()
handleNotFound = do
  status status404
  let msg :: Text
      msg = "Path not found"
  json $ object ["error" .= msg]

application :: ScottyT AppError App ()
application = do
  defaultHandler handleException
  post "/cart/:cartId/purchase" postCartPurchaseHandler
  notFound handleNotFound

main :: IO ()
main = do
  let port = 3000
  scottyT port runApp application
