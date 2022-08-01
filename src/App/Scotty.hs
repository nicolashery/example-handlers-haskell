module App.Scotty
  ( main,
  )
where

import Web.Scotty (ScottyM, get, param, scotty, text)

application :: ScottyM ()
application = do
  get "/echo/:message" $ do
    message <- param "message"
    text message

main :: IO ()
main = do
  let port = 3000
  scotty port application
