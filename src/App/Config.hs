module App.Config
  ( Config
      ( Config,
        configPaymentMaxRetries,
        configPaymentRetryDelay,
        configBookingUrl,
        configPaymentUrl
      ),
    configInit,
  )
where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { configPaymentMaxRetries :: Int,
    configPaymentRetryDelay :: Int,
    configBookingUrl :: Text,
    configPaymentUrl :: Text
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
  bookingUrl <- readEnvDefault "http://localhost:3001/booking" "BOOKING_URL"
  paymentUrl <- readEnvDefault "http://localhost:3001/payment" "PAYMENT_URL"
  let config =
        Config
          { configPaymentMaxRetries = paymentMaxRetries,
            configPaymentRetryDelay = paymentRetryDelay,
            configBookingUrl = bookingUrl,
            configPaymentUrl = paymentUrl
          }
  pure config
