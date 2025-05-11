{-# LANGUAGE OverloadedStrings #-}

module Lemmy.Utilities where

import Network.HTTP.Simple
    ( parseRequest,
      Request,
      getResponseBody,
      httpLBS,
      setRequestBodyJSON,
      setRequestHeader,
      setRequestMethod )
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- Define data types for the API responses
data LoginResponse = LoginResponse
    { jwt :: Text
    , registrationCreated :: Bool
    , verifyEmailSent :: Bool
    } deriving (Show)

-- Parse JSON responses
instance FromJSON LoginResponse where
    parseJSON = withObject "LoginResponse" $ \v -> LoginResponse
        <$> v .: "jwt"
        <*> v .: "registration_created"
        <*> v .: "verify_email_sent"

setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["Haskell Network.HTTP.Simple client version 0.1"]

-- Login to Lemmy and get a JWT token
loginToLemmy :: Text -> Text -> Text -> IO (Either String LoginResponse)
loginToLemmy instanceUrl user password = do
    let loginUrl = T.unpack instanceUrl <> "/api/v3/user/login"
    let requestBody = object [ "username_or_email" .= user, "password" .= password]
    request <- parseRequest loginUrl
    let req = setRequestBodyJSON requestBody
            $ setRequestMethod "POST"
            $ setUserAgent
            request

    response <- httpLBS req
    let responseBody = getResponseBody response
    return $ eitherDecode responseBody
