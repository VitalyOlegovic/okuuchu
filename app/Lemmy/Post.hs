{-# LANGUAGE OverloadedStrings #-}

module Lemmy.Post where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Lemmy.PostRequest as PR
import Lemmy.PostResponse


createPostIO :: T.Text -> T.Text -> Double -> String -> Maybe String -> IO(Either String PostResponse)
createPostIO instanceUrl jwtToken communityIdentifier postName postURL  = do
    let postRequest = PR.PostRequest
            { PR.scheduledPublishTime = Nothing
            , PR.tags = Just []
            , PR.customThumbnail = Nothing
            , PR.languageId = Nothing
            , PR.nsfw = Just False
            , PR.honeypot = Nothing
            , PR.altText = Nothing
            , PR.body = Nothing
            , PR.url = postURL
            , PR.communityId = communityIdentifier
            , PR.name = postName
            }
    let apiUrl = T.unpack instanceUrl <> "/api/v3/post"
    let requestBody = encode postRequest
    print requestBody
    request <- parseRequest apiUrl
    let req = setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 jwtToken]
            $ setRequestBodyLBS requestBody
            $ setRequestMethod "POST"
            $ setRequestHeader "User-Agent" ["haskell-client"]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["application/json"]
            $ request

    response <- httpLBS req
    print $ "Status code: " ++ show (getResponseStatusCode response)
    let responseBody = getResponseBody response
    print $ "Raw response: " <> show responseBody 
    return $ eitherDecode responseBody