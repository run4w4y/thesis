module NotesApp.Conduit.Options
  ( Options(..)
  , getOptions
  ) where

import qualified Data.ByteString.Char8 as ByteString
import Options.Applicative
  ( Parser
  , auto
  , customExecParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , prefs
  , short
  , showHelpOnError
  , str
  , value
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { port :: Int
  , databaseUrl :: ByteString.ByteString
  } deriving (Show)

optionsParser :: Maybe Int -> Maybe String -> Parser Options
optionsParser defaultPort defaultDatabaseUrl =
  Options
    <$> portParser
    <*> databaseUrlParser
  where
    portParser =
      option auto
        ( maybe (value 8080) value defaultPort
        <> short 'p'
        <> long "port"
        <> metavar "PORT"
        )
    databaseUrlParser =
      ByteString.pack <$> option str
        ( maybe mempty value defaultDatabaseUrl
        <> short 'd'
        <> long "database-url"
        <> help "Of the form postgres://user:password@hostname:port/name"
        <> metavar "DATABASE_URL"
        )

getOptions :: IO Options
getOptions = do
  port <- getPort
  databaseUrl <- getDatabaseUrl
  customExecParser
    (prefs showHelpOnError)
    (info
       (helper <*> optionsParser port databaseUrl)
       (header "NotesApp" <> fullDesc))
  where
    getPort = (readMaybe =<<) <$> lookupEnv "PORT"
    getDatabaseUrl = lookupEnv "DATABASE_URL"