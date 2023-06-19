module NotesApp.Conduit.Notes.Database.Note
  ( NoteT(..)
  , Note
  , NoteId
  , PrimaryKey(..)
  ) where

import           Data.Text     (Text)
import           Database.Beam (Beamable, Columnar, Identity, PrimaryKey,
                                Table (..))
import           GHC.Generics  (Generic)
import           GHC.Int       (Int32)
import           Prelude       hiding (id)

data NoteT f = Note
  { id      :: Columnar f Int32
  , title   :: Columnar f Text
  , content :: Columnar f Text
  }

deriving instance Generic (NoteT f)
deriving instance Beamable NoteT

type Note = NoteT Identity

deriving instance Show Note
deriving instance Eq Note

instance Table NoteT where
  data PrimaryKey NoteT f = NoteId
    { unNoteId :: Columnar f Int32
    } deriving (Generic, Beamable)
  primaryKey = NoteId . id

type NoteId = PrimaryKey NoteT Identity

deriving instance Show NoteId
deriving instance Eq NoteId
