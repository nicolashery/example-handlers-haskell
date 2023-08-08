module App.Db
  ( HasDbPool (getDbPool),
    dbInit,
    withConn,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import UnliftIO (MonadUnliftIO)

class HasDbPool env where
  getDbPool :: env -> Pool Connection

dbInit :: Text -> IO (Pool Connection)
dbInit databaseUrl = do
  newPool $
    defaultPoolConfig
      create
      destroy
      poolTtl
      poolSize
  where
    create = connectPostgreSQL (cs databaseUrl)
    destroy = close
    poolTtl = 10
    poolSize = 10

withConn ::
  (MonadReader env m, HasDbPool env, MonadUnliftIO m) => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getDbPool
  liftIO $ withResource pool action
