{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Scotty
  ( main,
  )
where

import App.Cart
  ( BookingId (unBookingId),
    CartException (CartException),
    CartId (CartId, unCartId),
    CartStatus (CartStatusLocked, CartStatusOpen, CartStatusPurchased),
    HasCartConfig (getBookingUrl, getPaymentUrl),
    PaymentId (unPaymentId),
    getCartStatus,
    processBooking,
    processPayment,
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
import App.Text (tlshow)
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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Pool (Pool)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Database.PostgreSQL.Simple (Connection)
import Lens.Micro (lens)
import Network.HTTP.Req
  ( HttpConfig,
    MonadHttp (getHttpConfig, handleHttpException),
    defaultHttpConfig,
  )
import Network.HTTP.Types (status404, status409, status500)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catch)
import Web.Scotty.Trans
  ( ActionT,
    ScottyError,
    ScottyT,
    param,
    post,
    raiseStatus,
    scottyT,
    text,
  )

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
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App, MonadUnliftIO)

instance MonadLogger AppM where
  monadLoggerLog loc logSource logLevel msg =
    AppM $ lift $ monadLoggerLog loc logSource logLevel msg

instance (ScottyError e, MonadLogger m) => MonadLogger (ActionT e m) where
  monadLoggerLog loc logSource logLevel msg =
    lift $ monadLoggerLog loc logSource logLevel msg

instance MonadHttp AppM where
  handleHttpException = throwIO
  getHttpConfig = asks appHttpConfig

runApp :: AppM a -> IO a
runApp m = do
  app <- appInit
  runLoggerLoggingT app $ runReaderT (unAppM m) app

postCartPurchaseHandler :: ActionT TL.Text AppM ()
postCartPurchaseHandler = do
  cartId <- CartId <$> param "cartId"
  cartStatusMaybe <- lift $ getCartStatus cartId
  case cartStatusMaybe of
    Nothing -> do
      logWarn $ "Cart does not exist" :# ["cart_id" .= cartId]
      raiseStatus status404 "Cart does not exist"
    Just CartStatusPurchased -> do
      logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
      raiseStatus status409 "Cart already purchased"
    Just CartStatusLocked -> do
      logWarn $ "Cart locked" :# ["cart_id" .= cartId]
      raiseStatus status409 "Cart locked"
    Just CartStatusOpen -> do
      logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
      paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
      let action :: AppM (Either Text (BookingId, PaymentId))
          action = Right <$> concurrently (processBooking cartId) (processPayment cartId)
          handler :: CartException -> AppM (Either Text (BookingId, PaymentId))
          handler (CartException msg) = pure $ Left msg
      result <- lift $ catch action handler
      case result of
        Left msg -> do
          logWarn $ ("Cart purchase failed: " <> msg) :# ["cart_id" .= cartId]
          raiseStatus status500 (cs $ "Cart purchase failed: " <> msg)
        Right (bookingId, paymentId) -> do
          liftIO $ threadDelay (100 * 1000)
          let response =
                mconcat
                  [ "cartId: ",
                    cs $ unCartId cartId,
                    "\n",
                    "paymentMaxRetries: ",
                    tlshow paymentMaxRetries,
                    "\n",
                    "bookingId: ",
                    cs $ unBookingId bookingId,
                    "\n",
                    "paymentId: ",
                    cs $ unPaymentId paymentId,
                    "\n"
                  ]
          logInfo $ "Cart purchase successful" :# ["cart_id" .= cartId]
          text response

application :: ScottyT TL.Text AppM ()
application = do
  post "/cart/:cartId/purchase" postCartPurchaseHandler

main :: IO ()
main = do
  let port = 3000
  scottyT port runApp application
