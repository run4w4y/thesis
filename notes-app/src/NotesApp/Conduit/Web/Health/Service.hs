module NotesApp.Conduit.Web.Health.Service
  ( Service(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (NominalDiffTime)
import Data.Text (Text)
import GHC.Generics (Generic)

data Service = Service
  { title :: Text
  , status :: Bool
  , duration :: NominalDiffTime
  } deriving (Generic)

deriving instance ToJSON Service
deriving instance ToSchema Service