module App.Config
  ( Config
      ( Config,
        configBookingDelay,
        configPaymentDelay,
        configPurchaseDelay,
        configBookingUrl,
        configPaymentUrl,
        configDatabaseUrl
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
  { configBookingDelay :: Int,
    configPaymentDelay :: Int,
    configPurchaseDelay :: Int,
    configBookingUrl :: Text,
    configPaymentUrl :: Text,
    configDatabaseUrl :: Text
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
  bookingDelay <- readEnvDefault (200 * 1000) "BOOKING_DELAY"
  paymentDelay <- readEnvDefault (100 * 1000) "PAYMENT_DELAY"
  purchaseDelay <- readEnvDefault (1 * 1000) "PURCHASE_DELAY"
  bookingUrl <- readEnvDefault "http://localhost:3001/booking" "BOOKING_URL"
  paymentUrl <- readEnvDefault "http://localhost:3001/payment" "PAYMENT_URL"
  databaseUrl <- readEnvDefault "postgresql://example_haskell:example_haskell@localhost/example_haskell" "DATABASE_URL"
  let config =
        Config
          { configBookingDelay = bookingDelay,
            configPaymentDelay = paymentDelay,
            configPurchaseDelay = purchaseDelay,
            configBookingUrl = bookingUrl,
            configPaymentUrl = paymentUrl,
            configDatabaseUrl = databaseUrl
          }
  pure config
