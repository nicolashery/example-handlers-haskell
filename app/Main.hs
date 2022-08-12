module Main (main) where

import App.Servant qualified

main :: IO ()
main = do
  App.Servant.main
