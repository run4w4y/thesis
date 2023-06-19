module NotesApp.Conduit.Web.Errors
  ( ErrorBody(..)
  , failedValidation
  , forbidden
  , internalServerError
  , notAuthorized
  , notFound
  ) where

import Servant (err401, err403, err404, err422, err500, errBody, ServerError)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import GHC.Generics (Generic)

data ErrorBody errors = ErrorBody
  { message :: Text
  , errors :: Maybe errors
  } deriving (Generic)

deriving instance ToJSON errors => ToJSON (ErrorBody errors)

notAuthorized :: ServerError
notAuthorized =
  err401 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Not Authorized"
        , errors = Nothing
        }

forbidden :: ServerError
forbidden =
  err403 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Forbidden"
        , errors = Nothing
        }

notFound :: Text -> ServerError
notFound resourceName =
  err404 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = resourceName <> " not found"
        , errors = Nothing
        }

failedValidation :: ToJSON failures => failures -> ServerError
failedValidation failures =
  err422 {errBody = encode (body failures)}
    where
      body fs = ErrorBody
        { message = "Failed validation"
        , errors = Just fs
        }

internalServerError :: Text -> ServerError
internalServerError message =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [message]
        }