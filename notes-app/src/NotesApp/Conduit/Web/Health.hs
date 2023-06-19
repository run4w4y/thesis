module NotesApp.Conduit.Web.Health
  ( server
  , HealthAPI
  ) where

import           Control.Exception                   (SomeException, catch)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (ToJSON)
import           Data.Swagger                        (ToSchema)
import           Data.Text                           (Text)
import           Data.Time                           (diffUTCTime,
                                                      getCurrentTime)
import           Database.PostgreSQL.Simple          (Connection, Only, query_)
import           GHC.Generics                        (Generic)
import           NotesApp.Conduit.Environment        (Environment (..))
import           NotesApp.Conduit.Web.Health.Service (Service (Service))
import qualified NotesApp.Conduit.Web.Health.Service as Service
import           Servant                             (Handler, Server)
import           Servant.API                         (Get, JSON, (:>))

data Status = Status
  { title    :: Text
  , status   :: Bool
  , services :: [Service]
  } deriving (Generic)

deriving instance ToJSON Status
deriving instance ToSchema Status

type HealthAPI = "health" :> Get '[ JSON] Status

checkDatabase :: Environment -> IO Service
checkDatabase Environment {withDatabaseConnection} = do
  start <- getCurrentTime
  status <- withDatabaseConnection check `catch` failCheck
  end <- getCurrentTime
  pure
    Service
      { Service.title = "Database"
      , Service.status
      , Service.duration = diffUTCTime end start
      }
  where
    failCheck :: SomeException -> IO Bool
    failCheck = const (pure False)
    check :: Connection -> IO Bool
    check conn =
      True <$ (query_ conn "select 1" :: IO [Only Int])

health :: Environment -> Handler Status
health environment = do
  services <- liftIO $ sequence [checkDatabase environment]
  pure
    Status
      { title = "Realworld Conduit Api"
      , status = all Service.status services
      , services
      }

server :: Environment -> Server HealthAPI
server = health
