{-# LANGUAGE OverloadedStrings #-}

module Database.PostDB where

import Control.Monad (void)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Feeds as F
import Config
import Data.Time.Clock (getCurrentTime, UTCTime)

data PostEntity = PostEntity
  { postId :: Maybe Int,
    feedId :: Int,
    lemmyCommunity :: T.Text,
    lemmyInstanceName :: T.Text,
    datetime :: UTCTime
  }
  deriving (Show)

instance FromRow PostEntity where
  fromRow = PostEntity <$> field <*> field <*> field <*> field <*> field

instance ToRow PostEntity where
  toRow (PostEntity mId feedId' community instance' dt) = 
    toRow (mId, feedId', community, instance', dt)

-- Initialize the database table
initializeDB :: Connection -> IO ()
initializeDB conn = void $ execute_ conn 
  "CREATE TABLE IF NOT EXISTS posts \
  \ (id INTEGER PRIMARY KEY AUTOINCREMENT, \
  \ feed_id INTEGER NOT NULL, \
  \ lemmy_community TEXT NOT NULL, \
  \ lemmy_instance TEXT NOT NULL, \
  \ datetime TIMESTAMP NOT NULL)"

-- Insert a new post
insertPost :: Connection -> PostEntity -> IO Int64
insertPost conn post = do
  execute 
    conn 
    "INSERT INTO posts (feed_id, lemmy_community, lemmy_instance, datetime) \
    \ VALUES (?, ?, ?, ?)" 
    (feedId post, lemmyCommunity post, lemmyInstanceName post, datetime post)
  lastInsertRowId conn -- Returns the ID of the newly inserted row

-- Get all posts
getAllPosts :: Connection -> IO [PostEntity]
getAllPosts conn = query_ conn "SELECT * FROM posts"

-- Get posts by feed ID
getPostsByFeedId :: Connection -> Int -> IO [PostEntity]
getPostsByFeedId conn fid = query conn 
  "SELECT * FROM posts WHERE feed_id = ?" (Only fid)

-- Get post by ID
getPostById :: Connection -> Int -> IO (Maybe PostEntity)
getPostById conn pid = do
  results <- query conn "SELECT * FROM posts WHERE id = ?" (Only pid)
  case results of
    [] -> return Nothing
    (x:_) -> return (Just x)

-- Update a post
updatePost :: Connection -> PostEntity -> IO ()
updatePost conn post = case postId post of
  Nothing -> error "Cannot update post without ID"
  Just pid -> void $ execute conn 
    "UPDATE posts SET feed_id = ?, lemmy_community = ?, lemmy_instance = ?, datetime = ? \
    \ WHERE id = ?"
    (feedId post, lemmyCommunity post, lemmyInstanceName post, datetime post, pid)

-- Delete a post
deletePost :: Connection -> Int -> IO ()
deletePost conn pid = void $ execute conn "DELETE FROM posts WHERE id = ?" (Only pid)

-- Create a new post entity (helper function)
createPost :: Int -> T.Text -> T.Text -> UTCTime -> PostEntity
createPost fid community instance' dt = 
  PostEntity Nothing fid community instance' dt
