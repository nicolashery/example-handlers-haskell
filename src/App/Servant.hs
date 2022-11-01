{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Servant (main) where

import App.Cart
  ( BookingId (unBookingId),
    CartException (CartException),
    CartId (CartId, unCartId),
    CartStatus (CartStatusLocked, CartStatusOpen, CartStatusPurchased),
    HasCartConfig (getBookingUrl, getPaymentUrl),
    PaymentId (unPaymentId),
    getCartStatus,
    markCartAsPurchased,
    processBooking,
    processPayment,
    withCart,
  )
import App.Config
  ( Config
      ( configBookingUrl,
        configDatabaseUrl,
        configPaymentMaxRetries,
        configPaymentUrl
      ),
    configInit,
  )
import App.Db (HasDbPool (getDbPool), dbInit)
import App.Text (tshow)
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
import Data.Pool (Pool)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Lens.Micro (lens)
import Network.HTTP.Req
  ( HttpConfig,
    MonadHttp (getHttpConfig, handleHttpException),
    defaultHttpConfig,
  )
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    FromHttpApiData,
    Handler (Handler),
    HasServer (ServerT),
    PlainText,
    Post,
    Proxy (Proxy),
    ServerError (errBody),
    err404,
    err409,
    err500,
    hoistServer,
    serve,
    (:>),
  )
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
  getPaymentUrl = configPaymentUrl . appConfig

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

type Api = "cart" :> Capture "cartId" CartId :> "purchase" :> Post '[PlainText] Text

deriving instance FromHttpApiData CartId

postCartPurchaseHandler :: CartId -> AppM Text
postCartPurchaseHandler cartId = do
  cartStatusMaybe <- getCartStatus cartId
  case cartStatusMaybe of
    Nothing -> do
      logWarn $ "Cart does not exist" :# ["cart_id" .= cartId]
      throwIO $ err404 {errBody = "Cart does not exist"}
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      throwIO $ err409 {errBody = "Cart already purchased"}
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      throwIO $ err409 {errBody = "Cart locked"}
    Just CartStatusOpen -> do
      withCart cartId $ do
        logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
        paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
        let action :: AppM (Either Text (BookingId, PaymentId))
            action = Right <$> concurrently (processBooking cartId) (processPayment cartId)
            handleError :: CartException -> AppM (Either Text (BookingId, PaymentId))
            handleError (CartException msg) = pure $ Left msg
        result <- catch action handleError
        case result of
          Left msg -> do
            logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
            throwIO $ err500 {errBody = cs $ "Cart purchase failed: " <> msg}
          Right (bookingId, paymentId) -> do
            liftIO $ threadDelay (100 * 1000)
            let response =
                  mconcat
                    [ "cartId: ",
                      unCartId cartId,
                      "\n",
                      "paymentMaxRetries: ",
                      tshow paymentMaxRetries,
                      "\n",
                      "bookingId: ",
                      unBookingId bookingId,
                      "\n",
                      "paymentId: ",
                      unPaymentId paymentId,
                      "\n"
                    ]
            markCartAsPurchased cartId
            logInfo $ "Cart purchase successful" :# ["cart_id" .= cartId]
            pure response

server :: ServerT Api AppM
server = postCartPurchaseHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  app <- appInit
  let port = 3000
      waiApp = serve api $ hoistServer api (appToHandler app) server
  run port waiApp
