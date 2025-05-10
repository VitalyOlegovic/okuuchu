{-# LANGUAGE OverloadedStrings #-}

module Lemmy where

import Network.HTTP.Simple
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Config as C
import PasswordManager


-- Define data types for the API responses
data LoginResponse = LoginResponse
    { jwt :: Text
    , registrationCreated :: Bool
    , verifyEmailSent :: Bool
    } deriving (Show)

data UserDetails = UserDetails
    { userId :: Int
    , username :: Text
    , email :: Maybe Text
    } deriving (Show)

-- Parse JSON responses
instance FromJSON LoginResponse where
    parseJSON = withObject "LoginResponse" $ \v -> LoginResponse
        <$> v .: "jwt"
        <*> v .: "registration_created"
        <*> v .: "verify_email_sent"

instance FromJSON UserDetails where
    parseJSON = withObject "UserDetails" $ \v -> do
        user <- v .: "person_view" >>= (.: "person")
        UserDetails
            <$> user .: "id"
            <*> user .: "name"
            <*> user .:? "email"


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


entrypoint :: T.Text -> C.Config -> IO ()
entrypoint instanceId config = do
    -- Get password (fixed to pass instanceId)
    password <- managePasswordIO instanceId config
    
    let instanceUrl = C.getInstanceUrl instanceId config
        myUsername = C.getInstanceUsername instanceId config
        myPassword = T.pack password

    -- Step 1: Log in and get JWT
    case (instanceUrl, myUsername) of
        (Right instanceUrlText, Right myUsernameText) -> do
            loginResult <- loginToLemmy instanceUrlText myUsernameText myPassword
            case loginResult of
                Left err -> putStrLn $ "Login failed: " <> err
                Right loginResponse -> do
                    putStrLn "Logged in."

                    -- Step 2: Fetch user details
                    userResult <- fetchUserDetails instanceUrlText (jwt loginResponse) myUsernameText
                    case userResult of
                        Left err -> putStrLn $ "Failed to fetch user: " <> err
                        Right userDetails -> do
                            putStrLn "User Details:"
                            putStrLn $ "- ID: " <> show (userId userDetails)
                            putStrLn $ "- Username: " <> T.unpack (username userDetails)
                            putStrLn $ "- Email: " <> maybe "None" T.unpack (email userDetails)
        
        (Left urlErr, _) -> putStrLn $ "Invalid instance URL: " <> T.unpack urlErr
        (_, Left userErr) -> putStrLn $ "Invalid username: " <> T.unpack userErr