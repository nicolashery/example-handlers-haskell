{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Servant (main) where

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
    LoggingT,
    Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    logWarn,
    runLoggerLoggingT,
    (.=),
  )
import Blammo.Logging.Simple (newLoggerEnv)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON (toJSON), encode, object)
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

instance HasDbPool App where
  getDbPool = appDbPool

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

newtype AppM a = AppM
  { unAppM :: ReaderT App (LoggingT IO) a
  }
  deriving (Functor, Applicative, Monad, MonadReader App, MonadIO, MonadUnliftIO)

instance MonadLogger AppM where
  monadLoggerLog loc logSource logLevel msg =
    AppM $ lift $ monadLoggerLog loc logSource logLevel msg

instance MonadHttp AppM where
  handleHttpException = throwIO
  getHttpConfig = asks appHttpConfig

appToHandler :: App -> AppM a -> Handler a
appToHandler app m =
  Handler $ ExceptT $ try $ runLoggerLoggingT app $ runReaderT (unAppM m) app

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

postCartPurchaseHandler :: CartId -> AppM CartPurchaseResponse
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
        let action :: AppM (Either Text (BookingId, PaymentId))
            action = Right <$> concurrently (processBooking cartId) (processPayment cartId)
            handleError :: CartException -> AppM (Either Text (BookingId, PaymentId))
            handleError (CartException msg) = pure $ Left msg
        result <- catch action handleError
        case result of
          Left msg -> do
            logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
            throwIO $ jsonError err500 ("Cart purchase failed: " <> msg)
          Right (bookingId, paymentId) -> do
            purchaseDelay <- asks (configPurchaseDelay . appConfig)
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

server :: ServerT Api AppM
server = postCartPurchaseHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  app <- appInit
  let port = 3000
      hoistedServer = hoistServer api (appToHandler app) server
      context = customErrorFormatters :. EmptyContext
      waiApp = serveWithContext api context hoistedServer
  run port waiApp
