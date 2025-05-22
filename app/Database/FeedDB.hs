{-# LANGUAGE OverloadedStrings #-}

module Database.FeedDB where

import Control.Monad (void)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Feeds as F
import Config

-- Data type representing a feed entry
data FeedEntryEntity = FeedEntryEntity
  { feedEntryId :: Maybe Int,
    title :: T.Text,
    date :: Maybe T.Text,
    content :: T.Text,
    links :: [T.Text]
  }
  deriving (Show)

-- For converting between SQL rows and FeedEntry
instance FromRow FeedEntryEntity where
  fromRow = FeedEntryEntity <$> field <*> field <*> field <*> field <*> (splitLinks <$> field)
    where
      splitLinks :: T.Text -> [T.Text]
      splitLinks = T.splitOn "|||"

instance ToRow FeedEntryEntity where
  toRow (FeedEntryEntity _ t d c ls) = toRow (t, d, c, T.intercalate "|||" ls)

-- Initialize the database and create table if needed
initializeDB :: FilePath -> IO Connection
initializeDB dbPath = do
  conn <- open dbPath
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS feed_entries \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \title TEXT NOT NULL, \
    \date TEXT, \
    \content TEXT NOT NULL, \
    \links TEXT NOT NULL)"
  return conn

-- Insert a new FeedEntry and return the last inserted row ID
insertFeedEntry :: Connection -> FeedEntryEntity -> IO Int64
insertFeedEntry conn entry = do
  execute
    conn
    "INSERT INTO feed_entries (title, date, content, links) VALUES (?, ?, ?, ?)"
    entry
  lastInsertRowId conn -- Returns the ID of the newly inserted row

-- Get all feed entries from the database
getAllFeedEntries :: Connection -> IO [FeedEntryEntity]
getAllFeedEntries conn = query_ conn "SELECT id, title, date, content, links FROM feed_entries"

-- Get feed entries by title (case insensitive partial match)
getFeedEntriesByTitle :: Connection -> T.Text -> IO [FeedEntryEntity]
getFeedEntriesByTitle conn titlePattern =
  query
    conn
    "SELECT title, date, content, links FROM feed_entries WHERE LOWER(title) LIKE LOWER(?)"
    (Only $ "%" <> titlePattern <> "%")

-- Update an existing feed entry by title
updateFeedEntry :: Connection -> Int -> FeedEntryEntity -> IO ()
updateFeedEntry conn feedId newEntry =
  void $
    execute
      conn
      "UPDATE feed_entries SET title = ?, date = ?, content = ?, links = ? WHERE id = ?"
      (title newEntry, date newEntry, content newEntry, T.intercalate "|||" (links newEntry), feedId)

-- Delete a feed entry by title
deleteFeedEntry :: Connection -> T.Text -> IO ()
deleteFeedEntry conn titleToDelete =
  void $
    execute
      conn
      "DELETE FROM feed_entries WHERE title = ?"
      (Only titleToDelete)

feedToDBEntry :: F.FeedEntryDTO -> FeedEntryEntity
feedToDBEntry feed =
  FeedEntryEntity
    { feedEntryId = Nothing,
      title = F.title feed,
      date = F.date feed,
      content = F.content feed,
      links = F.links feed
    }

dbEntryToFeed :: FeedEntryEntity -> F.FeedEntryDTO
dbEntryToFeed feed =
  F.FeedEntryDTO
    { F.title = title feed,
      F.date = date feed,
      F.content = content feed,
      F.links = links feed
    }

saveFeedEntry :: Config -> F.FeedEntryDTO -> IO Int64
saveFeedEntry config feedToConvert = do
  connection <- initializeDB $ getSQLiteFilePath config
  let feed = feedToDBEntry feedToConvert
  insertFeedEntry connection feed

readAllFeedEntries :: Config -> IO [F.FeedEntryDTO]
readAllFeedEntries config = do
  connection <- initializeDB $ getSQLiteFilePath config
  allFeedEntries <- getAllFeedEntries connection
  let convertedFeedEntries = fmap dbEntryToFeed allFeedEntries
  return convertedFeedEntries

