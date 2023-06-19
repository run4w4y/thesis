module NotesApp.Conduit.Web.API
  ( API
  , server
  ) where

import NotesApp.Conduit.Notes.Web (NotesAPI)
import qualified NotesApp.Conduit.Notes.Web as Notes

import NotesApp.Conduit.Web.Health (HealthAPI)
import qualified NotesApp.Conduit.Web.Health as Health
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))
import NotesApp.Conduit.Environment (Environment)

type API = HealthAPI :<|> NotesAPI

server :: Environment -> Server API
server env =
  Health.server env :<|>
  Notes.server env