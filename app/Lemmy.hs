{-# LANGUAGE OverloadedStrings #-}

module Lemmy where

import qualified Config as C
import PasswordManager ( managePasswordIO )
import Lemmy.Community
import Lemmy.Post
import Lemmy.User
import Lemmy.Utilities
import qualified Data.Text as T

manageCommunities :: T.Text -> T.Text -> IO()
manageCommunities instanceUrlText jwtToken = do
    communities <- fetchCommunitiesByName "EFGH" instanceUrlText
    case communities of
        Left err -> putStrLn $ "Cannot fetch communities by name: " <> err
        Right communityResponse -> do
            print communityResponse
            -- TODO extract community id and use it to post
            response <- createPostIO instanceUrlText jwtToken  36.0 "Haskell test post" (Just "https://example.com")
            print response

displayUserDetails :: T.Text -> LoginResponse -> T.Text -> IO ()
displayUserDetails instanceUrlText loginResponse myUsernameText = do
    userResult <- fetchUserDetails instanceUrlText (jwt loginResponse) myUsernameText
    case userResult of
        Left err -> putStrLn $ "Failed to fetch user: " <> err
        Right userDetails -> do
            putStrLn "User Details:"
            putStrLn $ "- ID: " <> show (userId userDetails)
            putStrLn $ "- Username: " <> T.unpack (username userDetails)
            putStrLn $ "- Email: " <> maybe "None" T.unpack (email userDetails)

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
                    displayUserDetails instanceUrlText loginResponse myUsernameText
                    manageCommunities instanceUrlText (jwt loginResponse)
        
        (Left urlErr, _) -> putStrLn $ "Invalid instance URL: " <> T.unpack urlErr
        (_, Left userErr) -> putStrLn $ "Invalid username: " <> T.unpack userErr