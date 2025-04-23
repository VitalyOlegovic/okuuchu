{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody)
import Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as FeedTypes
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS

-- Qualified imports for feed types
import qualified Text.RSS.Syntax as RSS
import qualified Text.Atom.Feed as Atom
import Text.Atom.Feed (TextContent(..))

main :: IO ()
main = do
    atomFeedExample <- fetchFeed "https://en.wikipedia.org/w/api.php?action=feedrecentchanges&feedformat=atom"

    case parseFeedString (BS.unpack atomFeedExample) of
        Just feed -> displayFeed feed
        Nothing -> putStrLn "Failed to parse the feed"

    rssFeedExample <- fetchFeed "http://feeds.bbci.co.uk/news/rss.xml"

    case parseFeedString (BS.unpack rssFeedExample) of
        Just feed -> displayFeed feed
        Nothing -> putStrLn "Failed to parse the feed"

-- | Fetch feed content from a URL
fetchFeed :: String -> IO BS.ByteString
fetchFeed url = do
    request <- parseRequest url
    response <- httpBS request
    return $ getResponseBody response

-- Updated displayFeed function with proper Atom feed handling
displayFeed :: FeedTypes.Feed -> IO ()
displayFeed feed = case feed of
    FeedTypes.RSSFeed rss -> do
        putStrLn "RSS Feed detected"
        let channel = RSS.rssChannel rss
        putStrLn $ "Title: " ++ T.unpack (RSS.rssTitle channel)
        mapM_ displayItem (RSS.rssItems channel)
    
    FeedTypes.AtomFeed atom -> do
        putStrLn "Atom Feed detected"
        -- Handle Atom feed title (which is TextContent)
        case Atom.feedTitle atom of
            TextString txt -> putStrLn $ "Title: " ++ T.unpack txt
            HTMLString txt -> putStrLn $ "Title (HTML): " ++ T.unpack txt
            XHTMLString _ -> putStrLn "Title: [XHTML content]"
        
        -- Display Atom entries
        putStrLn "\nEntries:"
        mapM_ displayAtomEntry (Atom.feedEntries atom)
    
    _ -> putStrLn "Unknown feed type"

-- Helper function to display RSS items
displayItem :: RSS.RSSItem -> IO ()
displayItem item = do
    case RSS.rssItemTitle item of
        Just title -> TIO.putStrLn $ "- " <> title
        Nothing -> putStrLn "- [No Title]"

-- Helper function to display Atom entries
displayAtomEntry :: Atom.Entry -> IO ()
displayAtomEntry entry = do
    -- Handle entry title (which is TextContent)
    let titleStr = case Atom.entryTitle entry of
                    TextString txt -> T.unpack txt
                    HTMLString txt -> T.unpack txt
                    XHTMLString _ -> "[XHTML content]"
    
    putStrLn $ "\n- Entry: " ++ titleStr
    
    -- Display other entry information
    case Atom.entryPublished entry of
        Just published -> putStrLn $ "  Published: " ++ T.unpack published
        Nothing -> return ()
    
    case Atom.entryUpdated entry of
        updated -> putStrLn $ "  Updated: " ++ T.unpack updated
    
    -- Display entry summary if available
    case Atom.entrySummary entry of
        Just (TextString txt) -> putStrLn $ "  Summary: " ++ T.unpack txt
        Just (HTMLString txt) -> putStrLn $ "  Summary (HTML): " ++ T.unpack txt
        Just (XHTMLString _) -> putStrLn "  Summary: [XHTML content]"
        Nothing -> return ()
    
    -- Display entry links
    unless (null (Atom.entryLinks entry)) $ do
        putStrLn "  Links:"
        mapM_ displayLink (Atom.entryLinks entry)

-- Helper function to display Atom links
displayLink :: Atom.Link -> IO ()
displayLink link = do
    putStrLn $ "    - " ++ T.unpack (Atom.linkHref link)
    case Atom.linkRel link of
        Nothing -> return ()  -- Default relation
        Just rel -> case rel of
            Left _ -> return ()
            Right relation -> putStrLn $ "      Relation: " ++ T.unpack relation
    case Atom.linkTitle link of
        Just title -> putStrLn $ "      Title: " ++ T.unpack title
        Nothing -> return ()