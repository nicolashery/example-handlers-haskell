module App.Config
  ( Config (Config, configPaymentMaxRetries, configPaymentRetryDelay),
    configInit,
  )
where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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
