module NotesApp.Conduit.Database.Monad
  ( MonadCRUD
  , MonadCRUD'
  , runCRUD
  , runCRUD'
  ) where

import           Conduit                    (ResourceT, runResourceT)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (MonadReader)
import           Database.PostgreSQL.Simple (Connection)
import           NotesApp.Conduit.Database  (QueryError (UnexpectedAmountOfRows))

-- without MonadError: for "safe" operations
type MonadCRUD' m =
  ( MonadReader Connection m
  , MonadIO m
  , MonadFail m
  )

type MonadCRUD m =
  ( MonadCRUD' m
  , MonadError QueryError m
  )

runCRUD' :: MonadIO m => ResourceT IO a -> m a
runCRUD' = liftIO . runResourceT

runCRUD :: (MonadIO m, MonadError QueryError m) => ResourceT IO (Maybe a) -> m a
runCRUD r = maybe (throwError (UnexpectedAmountOfRows 0)) pure =<< runCRUD' r
