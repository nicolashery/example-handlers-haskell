module App.Servant (main) where

import App.Config (Config (configPaymentMaxRetries), configInit)
import App.Text (tshow)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, Handler, HasServer (ServerT), PlainText, Post, Proxy (Proxy), hoistServer, serve, (:>))

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
  { unAppM :: ReaderT App Handler a
  }
  deriving (Functor, Applicative, Monad, MonadReader App)

appToHandler :: App -> AppM a -> Handler a
appToHandler app m = runReaderT (unAppM m) app

type Api = "cart" :> Capture "cartId" Text :> "purchase" :> Post '[PlainText] Text

postCartPurchaseHandler :: Text -> AppM Text
postCartPurchaseHandler cartId = do
  paymentMaxRetries <- asks (configPaymentMaxRetries . appConfig)
  let response =
        mconcat
          [ "cartId: ",
            cartId,
            "\n",
            "paymentMaxRetries: ",
            tshow paymentMaxRetries,
            "\n"
          ]
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
