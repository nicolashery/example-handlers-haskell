{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Scotty
  ( main,
  )
where

import App.Config (Config (configPaymentMaxRetries), configInit)
import App.Text (tlshow)
import Blammo.Logging
  ( HasLogger (loggerL),
    Logger,
    LoggingT,
    Message ((:#)),
    MonadLogger (monadLoggerLog),
    logInfo,
    runLoggerLoggingT,
    (.=),
  )
import Blammo.Logging.Simple (newLoggerEnv)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted.Safe (concurrently)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith, restoreM), StM)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Lens.Micro (lens)
import Web.Scotty.Trans (ActionT, ScottyError, ScottyT, param, post, scottyT, text)

data App = App
  { appConfig :: Config,
    appLogger :: Logger
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

appInit :: IO App
appInit = do
  config <- configInit
  logger <- newLoggerEnv
  let app =
        App
          { appConfig = config,
            appLogger = logger
          }
  pure app

newtype AppM a = AppM
  { unAppM :: ReaderT App (LoggingT IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App, MonadBase IO)

instance MonadBaseControl IO AppM where
  type StM AppM a = a
  liftBaseWith f = AppM $ liftBaseWith $ \runInBase -> f (runInBase . unAppM)
  restoreM = AppM . restoreM

instance MonadLogger AppM where
  monadLoggerLog loc logSource logLevel msg =
    AppM $ lift $ monadLoggerLog loc logSource logLevel msg

instance (ScottyError e, MonadLogger m) => MonadLogger (ActionT e m) where
  monadLoggerLog loc logSource logLevel msg =
    lift $ monadLoggerLog loc logSource logLevel msg

runApp :: AppM a -> IO a
runApp m = do
  app <- appInit
  runLoggerLoggingT app $ runReaderT (unAppM m) app

newtype BookingId = BookingId {unBookingId :: Text}

data BookingPayload = BookingPayload

newBookingPayload :: BookingPayload
newBookingPayload = BookingPayload

processBooking :: BookingPayload -> AppM BookingId
processBooking _ = do
  logInfo $ "Booking starting"
  liftIO $ threadDelay (2 * 1000 * 1000)
  let bookingId = BookingId "TKCY693D5ACB"
  logInfo $ "Booking successful" :# ["booking_id" .= unBookingId bookingId]
  pure bookingId

newtype PaymentId = PaymentId {unPaymentId :: Text}

data PaymentPayload = PaymentPayload

newPaymentPayload :: PaymentPayload
newPaymentPayload = PaymentPayload

processPayment :: PaymentPayload -> AppM PaymentId
processPayment _ = do
  logInfo $ "Payment starting"
  liftIO $ threadDelay (1 * 1000 * 1000)
  let paymentId = PaymentId "zTNBbSdy3vdOSnRT3xzFHviB"
  logInfo $ "Payment successful" :# ["payment_id" .= unPaymentId paymentId]
  pure paymentId

postCartPurchaseHandler :: ActionT TL.Text AppM ()
postCartPurchaseHandler = do
  cartId <- param "cartId"
  logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
  paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
  (bookingId, paymentId) <-
    lift $ concurrently (processBooking newBookingPayload) (processPayment newPaymentPayload)
  liftIO $ threadDelay (100 * 1000)
  let response =
        mconcat
          [ "cartId: ",
            cartId,
            "\n",
            "paymentMaxRetries: ",
            tlshow paymentMaxRetries,
            "\n",
            "bookingId: ",
            TL.fromStrict $ unBookingId bookingId,
            "\n",
            "paymentId: ",
            TL.fromStrict $ unPaymentId paymentId,
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
