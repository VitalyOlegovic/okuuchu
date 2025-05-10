{-# LANGUAGE OverloadedStrings #-}

import Feeds
import Lemmy
import qualified Data.ByteString.Char8 as BS
import Text.Feed.Import (parseFeedString)
import Config

main :: IO ()
main = do
    config <- loadConfig "config/app.yaml"
    case config of
        Left err -> putStrLn $ "Error loading config: " ++ err
        Right cfg -> do
            putStrLn "Successfully loaded config."

            Lemmy.entrypoint "test-instance" cfg

    {-
    atomFeedExample <- Feeds.fetchFeed "https://en.wikipedia.org/w/api.php?action=feedrecentchanges&feedformat=atom"

    
    case parseFeedString (BS.unpack atomFeedExample) of
        Just feed -> mapM_ print (feedToFeedEntries feed)
        Nothing -> putStrLn "Failed to parse the feed"

    rssFeedExample <- Feeds.fetchFeed "http://feeds.bbci.co.uk/news/rss.xml"

    case parseFeedString (BS.unpack rssFeedExample) of
        Just feed -> mapM_ print (feedToFeedEntries feed)
        Nothing -> putStrLn "Failed to parse the feed"
        -}
