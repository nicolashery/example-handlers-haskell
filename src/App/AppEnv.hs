module App.AppEnv
  ( AppEnv
      ( AppEnv,
        appEnvConfig,
        appEnvLogFunc,
        appEnvHttpConfig,
        appEnvDbPool
      ),
    appEnvInit,
  )
where

import App.Cart
  ( HasCartConfig
      ( getBookingDelay,
        getBookingUrl,
        getPaymentDelay,
        getPaymentUrl
      ),
  )
import App.Config
  ( Config
      ( configBookingDelay,
        configBookingUrl,
        configDatabaseUrl,
        configPaymentDelay,
        configPaymentUrl
      ),
    configInit,
  )
import App.Db (HasDbPool (getDbPool), dbInit)
import App.Logging (HasLogFunc (getLogFunc), LogFunc, loggingInit)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Req (HttpConfig, defaultHttpConfig)

data AppEnv = AppEnv
  { appEnvConfig :: Config,
    appEnvLogFunc :: LogFunc,
    appEnvHttpConfig :: HttpConfig,
    appEnvDbPool :: Pool Connection
  }

instance HasLogFunc AppEnv where
  getLogFunc = appEnvLogFunc

instance HasCartConfig AppEnv where
  getBookingUrl = configBookingUrl . appEnvConfig
  getBookingDelay = configBookingDelay . appEnvConfig
  getPaymentUrl = configPaymentUrl . appEnvConfig
  getPaymentDelay = configPaymentDelay . appEnvConfig

instance HasDbPool AppEnv where
  getDbPool = appEnvDbPool

appEnvInit :: IO AppEnv
appEnvInit = do
  config <- configInit
  logFunc <- loggingInit
  dbPool <- dbInit $ configDatabaseUrl config
  let app =
        AppEnv
          { appEnvConfig = config,
            appEnvLogFunc = logFunc,
            appEnvHttpConfig = defaultHttpConfig,
            appEnvDbPool = dbPool
          }
  pure app
