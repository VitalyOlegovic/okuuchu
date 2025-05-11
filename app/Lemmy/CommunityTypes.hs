{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lemmy.CommunityTypes where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Visibility = Public | Private | Local
    deriving (Show, Eq, Generic)

instance FromJSON Visibility
instance ToJSON Visibility

data SubscribedStatus = Subscribed | NotSubscribed | Pending
    deriving (Show, Eq, Generic)

instance FromJSON SubscribedStatus
instance ToJSON SubscribedStatus

data Community = Community
    { id :: Int
    , name :: Text
    , title :: Text
    , description :: Maybe Text
    , removed :: Bool
    , published :: UTCTime
    , updated :: Maybe UTCTime
    , deleted :: Bool
    , nsfw :: Bool
    , actor_id :: Text
    , local :: Bool
    , icon :: Maybe Text
    , hidden :: Bool
    , posting_restricted_to_mods :: Bool
    , instance_id :: Int
    , visibility :: Visibility
    } deriving (Show, Eq, Generic)

instance FromJSON Community
instance ToJSON Community

data Counts = Counts
    { community_id :: Int
    , subscribers :: Int
    , posts :: Int
    , comments :: Int
    , published :: UTCTime
    , users_active_day :: Int
    , users_active_week :: Int
    , users_active_month :: Int
    , users_active_half_year :: Int
    , subscribers_local :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON Counts
instance ToJSON Counts

data CommunityView = CommunityView
    { community :: Community
    , subscribed :: SubscribedStatus
    , blocked :: Bool
    , counts :: Counts
    , banned_from_community :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON CommunityView
instance ToJSON CommunityView

data Site = Site
    { id :: Int
    , name :: Text
    , sidebar :: Maybe Text
    , published :: UTCTime
    , updated :: Maybe UTCTime
    , icon :: Maybe Text
    , description :: Maybe Text
    , actor_id :: Text
    , last_refreshed_at :: UTCTime
    , inbox_url :: Text
    , public_key :: Text
    , instance_id :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON Site
instance ToJSON Site

data Person = Person
    { id :: Int
    , name :: Text
    , banned :: Bool
    , published :: UTCTime
    , updated :: Maybe UTCTime
    , actor_id :: Text
    , local :: Bool
    , deleted :: Bool
    , bot_account :: Bool
    , instance_id :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON Person
instance ToJSON Person

data Moderator = Moderator
    { community :: Community
    , moderator :: Person
    } deriving (Show, Eq, Generic)

instance FromJSON Moderator
instance ToJSON Moderator

data CommunityResponse = CommunityResponse
    { community_view :: CommunityView
    , site :: Site
    , moderators :: [Moderator]
    } deriving (Show, Eq, Generic)

instance FromJSON CommunityResponse
instance ToJSON CommunityResponse