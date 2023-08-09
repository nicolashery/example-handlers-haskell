module App.Logging
  ( LogFunc,
    HasLogFunc (getLogFunc),
    loggingInit,
    monadLoggerLogImpl,
  )
where

import Blammo.Logging (MonadLoggerIO (askLoggerIO), runLoggerLoggingT)
import Blammo.Logging.Simple (newLoggerEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
  ( Loc,
    LogLevel,
    LogSource,
    LogStr,
    ToLogStr (toLogStr),
  )
import Control.Monad.Reader (MonadReader, asks)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

class HasLogFunc env where
  getLogFunc :: env -> LogFunc

loggingInit :: IO LogFunc
loggingInit = do
  logger <- newLoggerEnv
  runLoggerLoggingT logger askLoggerIO

monadLoggerLogImpl ::
  (MonadReader env m, HasLogFunc env, MonadIO m, ToLogStr msg) =>
  Loc ->
  LogSource ->
  LogLevel ->
  msg ->
  m ()
monadLoggerLogImpl loc logSource logLevel msg = do
  logFunc <- asks getLogFunc
  liftIO $ logFunc loc logSource logLevel (toLogStr msg)
