{-# LANGUAGE OverloadedStrings #-}

module Lemmy.User where

import Network.HTTP.Simple
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import Lemmy.Utilities

data UserDetails = UserDetails
    { userId :: Int
    , username :: Text
    , email :: Maybe Text
    } deriving (Show)

instance FromJSON UserDetails where
    parseJSON = withObject "UserDetails" $ \v -> do
        user <- v .: "person_view" >>= (.: "person")
        UserDetails
            <$> user .: "id"
            <*> user .: "name"
            <*> user .:? "email"

-- Fetch user details using the JWT token
fetchUserDetails :: Text -> Text -> Text -> IO (Either String UserDetails)
fetchUserDetails instanceUrl jwtToken user = do
    let userUrl = T.unpack instanceUrl <> "/api/v3/user?username=" <> T.unpack user
    request <- parseRequest userUrl
    let req = setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 jwtToken]
                    $ setRequestMethod "GET" 
                    $ setUserAgent request
    response <- httpLBS req
    return $ eitherDecode (getResponseBody response)