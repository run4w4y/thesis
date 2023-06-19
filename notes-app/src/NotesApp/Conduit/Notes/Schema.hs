module NotesApp.Conduit.Notes.Schema
  ( CreateNote(..)
  , Note(..)
  , persistedToSchema
  ) where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Swagger                         (ToSchema)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import           GHC.Int                              (Int32)
import qualified NotesApp.Conduit.Notes.Database.Note as Persisted

data CreateNote = CreateNote
  { title   :: Text
  , content :: Text
  } deriving (ToJSON, ToSchema, FromJSON, Generic)

data Note = Note
  { id      :: Int32
  , title   :: Text
  , content :: Text
  } deriving (ToJSON, ToSchema, FromJSON, Generic)

persistedToSchema :: Persisted.Note -> Note
persistedToSchema note
  = Note
  { id = Persisted.id note
  , title = Persisted.title note
  , content = Persisted.content note
  }
