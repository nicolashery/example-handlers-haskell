{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Scotty
  ( main,
  )
where

import App.Cart
  ( BookingId (unBookingId),
    CartException (CartException),
    CartId (CartId, unCartId),
    PaymentId (unPaymentId),
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
    logWarn,
    runLoggerLoggingT,
    (.=),
  )
import Blammo.Logging.Simple (newLoggerEnv)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Lens.Micro (lens)
import Network.HTTP.Types (status409, status500)
import UnliftIO (MonadUnliftIO)
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
  cartId <- CartId <$> param "cartId"
  when (cartId == CartId "def456") $ do
    logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
    raiseStatus status409 "Cart already purchased"
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
      raiseStatus status500 ("Cart purchase failed: " <> TL.fromStrict msg)
    Right (bookingId, paymentId) -> do
      liftIO $ threadDelay (100 * 1000)
      let response =
            mconcat
              [ "cartId: ",
                TL.fromStrict $ unCartId cartId,
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
