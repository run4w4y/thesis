module NotesApp.Conduit.Monad
  ( MonadCRUD
  , MonadCRUD'
  ) where

import Control.Monad.Reader (MonadReader)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Except (MonadError)
import NotesApp.Conduit.Database (QueryError)

-- without MonadError: for "safe" operations
type MonadCRUD' m = 
  ( MonadReader Connection m
  , MonadIO m
  , MonadBaseControl IO m
  , MonadFail m
  )

type MonadCRUD m = 
  ( MonadCRUD' m
  , MonadError QueryError m
  )
