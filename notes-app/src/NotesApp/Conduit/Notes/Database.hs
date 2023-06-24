module NotesApp.Conduit.Notes.Database
  ( create
  , readAll
  , destroy
  , readOne
  ) where

import           Control.Monad                        (void)
import           Control.Monad.Reader                 (ask)
import           Data.Text                            (Text)
import           Database.Beam                        (Table (primaryKey))
import           Database.Beam.Postgres               (Postgres)
import           Database.Beam.Postgres.Conduit       (runDelete,
                                                       streamingRunInsertReturning,
                                                       streamingRunSelect)
import           Database.Beam.Postgres.Full          (PgInsertReturning,
                                                       insertReturning,
                                                       onConflictDefault)
import           Database.Beam.Query                  (Q, QExpr, all_, default_,
                                                       delete, guard_,
                                                       insertExpressions,
                                                       select, val_, (==.))
import           GHC.Int                              (Int32)
import           NotesApp.Conduit.Database            (ConduitDb (..),
                                                       conduitDb, maybeRow,
                                                       rowList)
import           NotesApp.Conduit.Database.Monad      (MonadCRUD, MonadCRUD',
                                                       runCRUD, runCRUD')
import qualified NotesApp.Conduit.Notes.Database.Note as Persisted

type NoteRow s = Persisted.NoteT (QExpr Postgres s)

readAll :: MonadCRUD' m => m [Persisted.Note]
readAll = do
  conn <- ask
  runCRUD'
    $ rowList
    $ streamingRunSelect conn
    $ select (all_ (conduitNotes conduitDb))

insertNote :: Text -> Text -> PgInsertReturning Persisted.Note
insertNote title content
  = insertReturning
    (conduitNotes conduitDb)
    (insertExpressions
      [ Persisted.Note
        { Persisted.id = default_
        , Persisted.title = val_ title
        , Persisted.content = val_ content
        }
      ]
    )
    onConflictDefault
    (Just id)

create :: MonadCRUD m => Text -> Text -> m Persisted.Note
create title content = do
  conn <- ask
  runCRUD 
    $ maybeRow 
    $ streamingRunInsertReturning conn 
    $ insertNote title content

readOne :: MonadCRUD m => Int32 -> m Persisted.Note
readOne noteId = do
  conn <- ask
  runCRUD 
    $ maybeRow 
    $ streamingRunSelect conn 
    $ select (selectNote noteId)

-- NOTE: maybe i couldve just used Database.Beam.Query.lookup_ here
selectNote :: Int32 -> Q Postgres ConduitDb s (NoteRow s)
selectNote noteId = do
  noteRow <- all_ (conduitNotes conduitDb)
  guard_ (Persisted.id noteRow ==. val_ noteId)
  pure noteRow

destroy :: MonadCRUD' m => Int32 -> m ()
destroy noteId = do
  conn <- ask
  void $ runDelete conn $ delete (conduitNotes conduitDb) $ \row ->
    primaryKey row ==. val_ (Persisted.NoteId noteId)

-- TODO: complete
-- update :: MonadCRUD m => Int -> Text -> Text -> m Persisted.Note
-- update noteId title content = do
--   conn <- ask
--   runUpdateReturning
--     conn
--     (updateReturning (conduitNotes conduitDb) )
