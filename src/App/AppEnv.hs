module App.AppEnv
  ( AppEnv
      ( AppEnv,
        appEnvConfig,
        appEnvLogger,
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
import Blammo.Logging (HasLogger (loggerL), Logger)
import Blammo.Logging.Simple (newLoggerEnv)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Lens.Micro (lens)
import Network.HTTP.Req (HttpConfig, defaultHttpConfig)

data AppEnv = AppEnv
  { appEnvConfig :: Config,
    appEnvLogger :: Logger,
    appEnvHttpConfig :: HttpConfig,
    appEnvDbPool :: Pool Connection
  }

instance HasLogger AppEnv where
  loggerL = lens appEnvLogger $ \x y -> x {appEnvLogger = y}

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
  logger <- newLoggerEnv
  dbPool <- dbInit $ configDatabaseUrl config
  let app =
        AppEnv
          { appEnvConfig = config,
            appEnvLogger = logger,
            appEnvHttpConfig = defaultHttpConfig,
            appEnvDbPool = dbPool
          }
  pure app
