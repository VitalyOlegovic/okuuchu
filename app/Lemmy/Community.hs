{-# LANGUAGE OverloadedStrings #-}

module Lemmy.Community where

import Network.HTTP.Simple
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Lemmy.CommunityTypes
import Lemmy.Utilities

fetchCommunitiesByName :: Text -> Text -> IO (Either String CommunityResponse)
fetchCommunitiesByName name instanceUrl = do
    let userUrl = T.unpack instanceUrl <> "/api/v3/community?name=" <> T.unpack name
    request <- parseRequest userUrl
    let req = setRequestMethod "GET" 
                    $ setUserAgent request
    response <- httpLBS req
    -- Print the raw response body
    BL.putStr (getResponseBody response)
    putStrLn " "
    return $ eitherDecode (getResponseBody response)