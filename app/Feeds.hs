{-# LANGUAGE OverloadedStrings #-}

module Feeds where

import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody)
import qualified Text.Feed.Types as FeedTypes
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe ( fromMaybe, maybeToList )

-- Qualified imports for feed types
import qualified Text.RSS.Syntax as RSS
import qualified Text.Atom.Feed as Atom
import Text.Atom.Feed (TextContent(..))
import qualified Text.Atom.Feed as Link

data FeedEntryDTO = FeedEntryDTO { title :: T.Text
                           , date :: Maybe T.Text
                           , content :: T.Text
                           , links :: [T.Text]
} deriving (Show)

-- | Fetch feed content from a URL
fetchFeed :: String -> IO BS.ByteString
fetchFeed url = do
    request <- parseRequest url
    response <- httpBS request
    return $ getResponseBody response

feedToFeedEntries :: FeedTypes.Feed -> [FeedEntryDTO]
feedToFeedEntries feed = case feed of
    FeedTypes.RSSFeed rss ->
        fmap feedItemToFeedEntry (RSS.rssItems channel)
        where channel = RSS.rssChannel rss

    FeedTypes.AtomFeed atom ->
        fmap atomItemToFeedEntry (Atom.feedEntries atom)

    FeedTypes.RSS1Feed _ -> []

    FeedTypes.XMLFeed _ -> []

feedItemToFeedEntry :: RSS.RSSItem -> FeedEntryDTO
feedItemToFeedEntry item = FeedEntryDTO {
    title = fromMaybe "- [No Title]" (RSS.rssItemTitle item)
    , date = RSS.rssItemPubDate item
    , content = fromMaybe "" (RSS.rssItemDescription item)
    , links = maybeToList $ RSS.rssItemLink item
        }

atomItemToFeedEntry :: Atom.Entry -> FeedEntryDTO
atomItemToFeedEntry entry = FeedEntryDTO {
    title = case Atom.entryTitle entry of
                    TextString txt -> txt
                    HTMLString txt -> txt
                    XHTMLString _ -> "[XHTML content]"
    , date =  Atom.entryPublished entry
    , content = case Atom.entrySummary entry of
        Just (TextString txt) -> txt
        Just (HTMLString txt) -> txt
        Just (XHTMLString _) -> "[XHTML content]"
        Nothing -> ""
    , links = Link.linkHref <$> Atom.entryLinks entry
}

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
        Just rssTitle -> TIO.putStrLn $ "- " <> rssTitle
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
        Just rssTitle -> putStrLn $ "      Title: " ++ T.unpack rssTitle
        Nothing -> return ()
