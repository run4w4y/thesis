module NotesApp.Conduit.Web
  ( app
  , context
  ) where

import Network.Wai (Application)
import NotesApp.Conduit.Environment (Environment(..))
import NotesApp.Conduit.Web.API (API)
import qualified NotesApp.Conduit.Web.API as API
import NotesApp.Conduit.Web.Swagger (NotesAppConduitSwagger)
import qualified NotesApp.Conduit.Web.Swagger as Swagger
import Servant
  ( (:<|>)((:<|>))
  , Context((:.), EmptyContext)
  , Server
  , serveWithContext
  , Proxy (Proxy)
  )
import Servant.Auth.Server (CookieSettings, defaultCookieSettings)

type NotesAppConduit =
  NotesAppConduitSwagger :<|>
  API

server :: Environment -> Server NotesAppConduit
server handle =
  Swagger.server :<|>
  API.server handle

notesAppConduit :: Proxy NotesAppConduit
notesAppConduit = Proxy

context :: Context '[CookieSettings]
context  =
  defaultCookieSettings :. EmptyContext

app :: Environment -> Application
app handle = serveWithContext notesAppConduit context (server handle)
