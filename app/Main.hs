module Main (main) where

import App.Scotty qualified

main :: IO ()
main = do
  App.Scotty.main
