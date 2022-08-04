module App.Scotty
  ( main,
  )
where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty.Trans (ActionT, ScottyT, param, post, scottyT, text)

tlshow :: (Show a) => a -> TL.Text
tlshow v = TL.pack $ show v

data Config = Config
  { configPaymentMaxRetries :: Int,
    configPaymentRetryDelay :: Int
  }

readEnvMaybe :: (Read a) => String -> IO (Maybe a)
readEnvMaybe env = do
  strMay <- lookupEnv env
  pure $ join (fmap readMaybe strMay)

readEnvDefault :: (Read a) => a -> String -> IO a
readEnvDefault def env = do
  valMay <- readEnvMaybe env
  pure $ fromMaybe def valMay

configInit :: IO Config
configInit = do
  paymentMaxRetries <- readEnvDefault 3 "PAYMENT_MAX_RETRIES"
  paymentRetryDelay <- readEnvDefault 2000 "PAYMENT_RETRY_DELAY"
  let config =
        Config
          { configPaymentMaxRetries = paymentMaxRetries,
            configPaymentRetryDelay = paymentRetryDelay
          }
  pure config

data App = App
  { appConfig :: Config,
    appLogFunc :: ()
  }

appInit :: IO App
appInit = do
  config <- configInit
  let app =
        App
          { appConfig = config,
            appLogFunc = ()
          }
  pure app

newtype AppM a = AppM
  { unAppM :: ReaderT App IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App)

runApp :: AppM a -> IO a
runApp m = do
  app <- appInit
  runReaderT (unAppM m) app

purchaseHandler :: ActionT TL.Text AppM ()
purchaseHandler = do
  cartId <- param "cartId"
  paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
  let response =
        mconcat
          [ "cartId: ",
            cartId,
            "\n",
            "paymentMaxRetries: ",
            tlshow paymentMaxRetries,
            "\n"
          ]
  text response

application :: ScottyT TL.Text AppM ()
application = do
  post "/cart/:cartId/purchase" purchaseHandler

main :: IO ()
main = do
  let port = 3000
  scottyT port runApp application
