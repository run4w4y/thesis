module NotesApp.Conduit.Web.Swagger
  ( server
  , NotesAppConduitSwagger
  ) where

import Control.Lens ((.~), (?~), (&))
import Data.Swagger (Swagger, description, info, title, version)
import NotesApp.Conduit.Web.API (API)
import Servant (Server, Proxy(Proxy))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type NotesAppConduitSwagger = SwaggerSchemaUI "swagger" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    & info . title .~ "NotesApp Conduit API"
    & info . version .~ "0.1.0.0"
    & info . description ?~ "Simple application for storing notes"

server :: Server NotesAppConduitSwagger
server = swaggerSchemaUIServer swaggerDoc