module App.Scotty
  ( main,
  )
where

import App.Config (Config (configPaymentMaxRetries), configInit)
import App.Text (tlshow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.Text.Lazy qualified as TL
import Web.Scotty.Trans (ActionT, ScottyT, param, post, scottyT, text)

data App = App
  { appConfig :: Config,
    appLogger :: ()
  }

appInit :: IO App
appInit = do
  config <- configInit
  let app =
        App
          { appConfig = config,
            appLogger = ()
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
