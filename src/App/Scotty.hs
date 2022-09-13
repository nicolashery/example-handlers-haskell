{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Scotty
  ( main,
  )
where

import App.Cart
  ( BookingId (unBookingId),
    PaymentId (unPaymentId),
    newBookingPayload,
    newPaymentPayload,
    processBooking,
    processPayment,
  )
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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Text.Lazy qualified as TL
import Lens.Micro (lens)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently)
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App, MonadUnliftIO)

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
