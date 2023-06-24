module NotesApp.Conduit.Database
  ( ConduitDb(..)
  , QueryError(..)
  , conduitDb
  , maybeRow
  , openConduitDb
  , rowList
  ) where

import           Control.Exception                    (Exception)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.ByteString.Char8                (ByteString)
import           Data.Conduit                         (ConduitT, (.|))
import qualified Data.Conduit                         as Conduit
import qualified Data.Conduit.List                    as Conduit
import           Database.Beam                        (Database,
                                                       DatabaseSettings,
                                                       TableEntity,
                                                       defaultDbSettings)
import           Database.Beam.Postgres               (Postgres)
import           Database.PostgreSQL.Simple           (Connection,
                                                       connectPostgreSQL)
import           GHC.Generics                         (Generic)
import           NotesApp.Conduit.Notes.Database.Note (NoteT)

data ConduitDb f = ConduitDb
  { conduitNotes :: f (TableEntity NoteT)
  } deriving (Generic)

instance Database Postgres ConduitDb

newtype QueryError = UnexpectedAmountOfRows Int
  deriving Show

instance Exception QueryError

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb = defaultDbSettings

openConduitDb :: MonadIO m => ByteString -> m Connection
openConduitDb = liftIO . connectPostgreSQL

maybeRow :: Monad m => ConduitT () a m () -> m (Maybe a)
maybeRow c = Conduit.runConduit (c .| Conduit.await)

rowList :: Monad m => ConduitT () a m () -> m [a]
rowList c = Conduit.runConduit (c .| Conduit.consume)
