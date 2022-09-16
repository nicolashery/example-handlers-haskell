{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Servant (main) where

import App.Cart
  ( CartId (CartId, unCartId),
  )
import App.Config (Config (configPaymentMaxRetries), configInit)
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
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Lens.Micro (lens)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    FromHttpApiData,
    Handler,
    HasServer (ServerT),
    PlainText,
    Post,
    Proxy (Proxy),
    ServerError (errBody),
    err409,
    hoistServer,
    serve,
    (:>),
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
  { unAppM :: ReaderT App (LoggingT Handler) a
  }
  deriving (Functor, Applicative, Monad, MonadReader App, MonadIO, MonadError ServerError)

instance MonadLogger AppM where
  monadLoggerLog loc logSource logLevel msg =
    AppM $ lift $ monadLoggerLog loc logSource logLevel msg

appToHandler :: App -> AppM a -> Handler a
appToHandler app m = runLoggerLoggingT app $ runReaderT (unAppM m) app

type Api = "cart" :> Capture "cartId" CartId :> "purchase" :> Post '[PlainText] Text

deriving instance FromHttpApiData CartId

postCartPurchaseHandler :: CartId -> AppM Text
postCartPurchaseHandler cartId = do
  when (cartId == CartId "def456") $ do
    logWarn $ "Cart already purchased" :# ["cart_id" .= cartId]
    throwError $ err409 {errBody = "Cart already purchased"}
  logInfo $ "Cart purchase starting" :# ["cart_id" .= cartId]
  paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
  liftIO $ threadDelay (100 * 1000)
  let response =
        mconcat
          [ "cartId: ",
            unCartId cartId,
            "\n",
            "paymentMaxRetries: ",
            tshow paymentMaxRetries,
            "\n"
          ]
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
