module App.Servant (main) where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant (Capture, Handler, PlainText, Post, Proxy (Proxy), Server, serve, (:>))

type Api = "cart" :> Capture "cartId" Text :> "purchase" :> Post '[PlainText] Text

postCartPurchaseHandler :: Text -> Handler Text
postCartPurchaseHandler cartId = do
  let response =
        mconcat
          [ "cartId: ",
            cartId,
            "\n"
          ]
  pure response

server :: Server Api
server = postCartPurchaseHandler

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  let port = 3000
      waiApp = serve api server
  run port waiApp
