module Main
  ( main
  ) where

import           Data.Aeson                                (ToJSON)
import           Data.Aeson.Text                           (encodeToLazyText)
import qualified Data.CaseInsensitive                      as CI
import           Data.Default                              (def)
import           Data.Map                                  (Map, singleton)
import           Data.Text.Lazy                            (unpack)
import           GHC.Generics                              (Generic)
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Cors               (cors, corsMethods,
                                                            corsRequestHeaders,
                                                            simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (CustomOutputFormatWithDetails),
                                                            RequestLoggerSettings (outputFormat),
                                                            mkRequestLogger)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified NotesApp.Conduit.Environment              as Environment
import           NotesApp.Conduit.Options                  (Options (port),
                                                            getOptions)
import           NotesApp.Conduit.Web                      (app)

data Event a = Event
  { event   :: String
  , payload :: a
  } deriving Generic

instance ToJSON a => ToJSON (Event a)

startupEvent :: Options -> Event (Map String String)
startupEvent options =
  Event {event = "startup", payload = singleton "port" (show (port options))}

jsonLogger :: IO Middleware
jsonLogger =
  mkRequestLogger
    def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

main :: IO ()
main = do
  options <- getOptions
  environment <- Environment.new options
  logger <- jsonLogger
  putStrLn $ unpack $ encodeToLazyText $ startupEvent options
  let resourcePolicy = simpleCorsResourcePolicy
        { corsRequestHeaders = [CI.mk "Content-Type", CI.mk "Authorization"]
        , corsMethods = ["PUT", "DELETE"]
        }
  run (port options) (logger . (cors . const . Just $ resourcePolicy ) $ app environment)
