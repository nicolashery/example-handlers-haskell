module App.Db
  ( HasDbPool (getDbPool),
    dbInit,
    withConn,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
  ( Pool,
    PoolConfig (PoolConfig, createResource, freeResource, poolCacheTTL, poolMaxResources),
    newPool,
    withResource,
  )
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)

class HasDbPool env where
  getDbPool :: env -> Pool Connection

dbInit :: Text -> IO (Pool Connection)
dbInit databaseUrl =
  let poolConfig =
        PoolConfig
          { createResource = connectPostgreSQL (cs databaseUrl),
            freeResource = close,
            poolCacheTTL = 10,
            poolMaxResources = 10
          }
   in newPool poolConfig

withConn ::
  (MonadReader env m, HasDbPool env, MonadIO m) => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getDbPool
  liftIO $ withResource pool action
