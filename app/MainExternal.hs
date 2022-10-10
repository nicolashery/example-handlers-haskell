module Main (main) where

import App.External qualified

main :: IO ()
main = do
  App.External.main
