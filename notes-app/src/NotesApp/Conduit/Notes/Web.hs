module NotesApp.Conduit.Notes.Web 
  ( server
  , NotesAPI
  ) where

import           Control.Monad.Except            (withExceptT)
import           Control.Monad.Reader            (runReaderT)
import qualified Data.Text                       as Text
import           GHC.Int                         (Int32)
import           NotesApp.Conduit.Environment    (Environment (..))
import qualified NotesApp.Conduit.Notes.Database as Database
import           NotesApp.Conduit.Notes.Schema   (CreateNote (CreateNote, content, title),
                                                  Note, persistedToSchema)
import           NotesApp.Conduit.Web.Errors     (internalServerError, notFound)
import           Servant                         (Capture, Get,
                                                  Handler (Handler), JSON,
                                                  NoContentVerb, Put, ReqBody,
                                                  Server, StdMethod (DELETE),
                                                  type (:<|>) (..), type (:>), NoContent(NoContent))


type CreateEndpoint =
  "api" :>
  "notes" :>
  ReqBody '[JSON] CreateNote :>
  Put '[JSON] Note

create :: Environment -> CreateNote -> Handler Note
create env (CreateNote {title, content}) =
  Handler
    $ withDatabaseConnection env
    $ \conn -> withExceptT (internalServerError . Text.pack . show)
    $ flip runReaderT conn
    $ persistedToSchema <$> Database.create title content

type DeleteEndpoint =
  "api" :>
  "notes" :>
  Capture "noteId" Int32 :>
  NoContentVerb 'DELETE

destroy :: Environment -> Int32 -> Handler NoContent
destroy env noteId = NoContent <$ destroyNote env noteId

destroyNote :: Environment -> Int32 -> Handler ()
destroyNote env noteId = 
  withDatabaseConnection env 
    $ runReaderT 
    $ Database.destroy noteId

type ReadAllEndpoint =
  "api" :>
  "notes" :>
  Get '[JSON] [Note]

readAll :: Environment -> Handler [Note]
readAll env =
  Handler
    $ withDatabaseConnection env
    $ runReaderT
    $ map persistedToSchema <$> Database.readAll

type ReadOneEndpoint =
  "api" :>
  "notes" :>
  Capture "noteId" Int32 :>
  Get '[JSON] Note

readOne :: Environment -> Int32 -> Handler Note
readOne env noteId =
  Handler
    $ withDatabaseConnection env
    $ \conn -> withExceptT (notFound . Text.pack . show)
    $ flip runReaderT conn
    $ persistedToSchema <$> Database.readOne noteId

type NotesAPI =
  CreateEndpoint  :<|>
  DeleteEndpoint  :<|>
  ReadAllEndpoint :<|>
  ReadOneEndpoint

server :: Environment -> Server NotesAPI
server env =
  create env :<|>
  destroy env :<|>
  readAll env :<|>
  readOne env
