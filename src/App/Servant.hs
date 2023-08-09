{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Servant (main) where

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
import App.Logging (monadLoggerLogImpl)
import Blammo.Logging
  ( Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    logWarn,
    (.=),
  )
import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Aeson (ToJSON (toJSON), encode, object)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( MonadHttp (getHttpConfig, handleHttpException),
  )
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    Context (EmptyContext, (:.)),
    ErrorFormatters (notFoundErrorFormatter),
    FromHttpApiData,
    Handler (Handler),
    HasServer (ServerT),
    JSON,
    Post,
    Proxy (Proxy),
    ServerError (errBody, errHeaders),
    defaultErrorFormatters,
    err404,
    err409,
    err500,
    hoistServer,
    serveWithContext,
    (:>),
  )
import Servant.Server (NotFoundErrorFormatter)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catch, throwIO, try)

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadReader AppEnv, MonadIO, MonadUnliftIO)

instance MonadLogger App where
  monadLoggerLog = monadLoggerLogImpl

instance MonadHttp App where
  handleHttpException = throwIO
  getHttpConfig = asks appEnvHttpConfig

appToHandler :: AppEnv -> App a -> Handler a
appToHandler app m =
  Handler $ ExceptT $ try $ runReaderT (unApp m) app

type Api = "cart" :> Capture "cartId" CartId :> "purchase" :> Post '[JSON] CartPurchaseResponse

deriving instance FromHttpApiData CartId

data CartPurchaseResponse = CartPurchaseResponse
  { cartPurchaseResponseCartId :: CartId,
    cartPurchaseResponsePurchaseDelay :: Int,
    cartPurchaseResponseBookingId :: BookingId,
    cartPurchaseResponsePaymentId :: PaymentId
  }
  deriving (Generic)

instance ToJSON CartPurchaseResponse where
  toJSON = defaultToJSON "cartPurchaseResponse"

postCartPurchaseHandler :: CartId -> App CartPurchaseResponse
postCartPurchaseHandler cartId = do
  cartStatusMaybe <- getCartStatus cartId
  case cartStatusMaybe of
    Nothing -> do
      logWarn $ "Cart does not exist" :# ["cart_id" .= cartId]
      throwIO $ jsonError err404 "Cart does not exist"
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      throwIO $ jsonError err409 "Cart already purchased"
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      throwIO $ jsonError err409 "Cart locked"
    Just CartStatusOpen -> do
      withCart cartId $ do
        logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
        let action :: App (Either Text (BookingId, PaymentId))
            action = Right <$> concurrently (processBooking cartId) (processPayment cartId)
            handleError :: CartException -> App (Either Text (BookingId, PaymentId))
            handleError (CartException msg) = pure $ Left msg
        result <- catch action handleError
        case result of
          Left msg -> do
            logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
            throwIO $ jsonError err500 ("Cart purchase failed: " <> msg)
          Right (bookingId, paymentId) -> do
            purchaseDelay <- asks (configPurchaseDelay . appEnvConfig)
            liftIO $ threadDelay purchaseDelay
            markCartAsPurchased cartId
            logInfo $ "Cart purchase successful" :# ["cart_id" .= cartId]
            pure $
              CartPurchaseResponse
                { cartPurchaseResponseCartId = cartId,
                  cartPurchaseResponsePurchaseDelay = purchaseDelay,
                  cartPurchaseResponseBookingId = bookingId,
                  cartPurchaseResponsePaymentId = paymentId
                }

jsonError :: ServerError -> Text -> ServerError
jsonError err msg =
  err
    { errBody = encode $ object ["error" .= msg],
      errHeaders = [("Content-Type", "application/json")]
    }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter _ =
  let msg :: Text
      msg = "Path not found"
   in err404
        { errBody = encode $ object ["error" .= msg],
          errHeaders = [("Content-Type", "application/json")]
        }

customErrorFormatters :: ErrorFormatters
customErrorFormatters =
  defaultErrorFormatters
    { notFoundErrorFormatter = notFoundFormatter
    }

server :: ServerT Api App
server = postCartPurchaseHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  app <- appEnvInit
  let port = 3000
      hoistedServer = hoistServer api (appToHandler app) server
      context = customErrorFormatters :. EmptyContext
      waiApp = serveWithContext api context hoistedServer
  run port waiApp
