{-# LANGUAGE RankNTypes #-}

module NotesApp.Conduit.Environment
  ( Environment(..)
  , new
  ) where

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Pool                   (Pool, createPool, withResource)
import           Database.PostgreSQL.Simple  (Connection, close)
import           NotesApp.Conduit.Database   (openConduitDb)
import           NotesApp.Conduit.Options    (Options)
import qualified NotesApp.Conduit.Options    as Options

data Environment = Environment
  { withDatabaseConnection :: forall a m. MonadBaseControl IO m =>
                                            (Connection -> m a) -> m a
  }

createDatabaseConnectionPool :: Options -> IO (Pool Connection)
createDatabaseConnectionPool options =
  createPool (openConduitDb (Options.databaseUrl options)) close 1 10 8

new :: Options -> IO Environment
new options = do
  databaseConnectionPool <- createDatabaseConnectionPool options
  pure
    Environment
      { withDatabaseConnection = withResource databaseConnectionPool
      }
