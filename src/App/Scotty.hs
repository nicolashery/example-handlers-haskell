module App.Scotty
  ( main,
  )
where

import Web.Scotty (ActionM, ScottyM, param, post, scotty, text)

purchaseHandler :: ActionM ()
purchaseHandler = do
  cartId <- param "cartId"
  text cartId

application :: ScottyM ()
application = do
  post "/cart/:cartId/purchase" purchaseHandler

main :: IO ()
main = do
  let port = 3000
  scotty port application
