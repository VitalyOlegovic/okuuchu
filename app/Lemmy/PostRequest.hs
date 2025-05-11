{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Lemmy.PostRequest where

import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON,
      camelTo2,
      encode,
      defaultOptions,
      genericToJSON,
      fieldLabelModifier,
      FromJSON,
      ToJSON(..),
      FromJSON(..) )
import Data.Aeson.Types (defaultOptions)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Text (Text)
import Data.Time (UTCTime)

-- Main request type
data PostRequest = PostRequest
  { scheduledPublishTime :: Maybe Double
  , tags                :: Maybe [Double]
  , customThumbnail     :: Maybe String
  , languageId          :: Maybe Double
  , nsfw                :: Maybe Bool
  , honeypot            :: Maybe String
  , altText             :: Maybe String
  , body                :: Maybe String
  , url                 :: Maybe String
  , communityId         :: Double  -- Required
  , name                :: String  -- Required
  } deriving (Show, Eq, Generic)

instance ToJSON PostRequest  where
  toJSON = genericToJSON defaultOptions 
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }


-- A smart constructor to ensure required fields are provided
mkPostRequest :: Double -> String -> PostRequest
mkPostRequest cid nm = PostRequest
  { scheduledPublishTime = Nothing
  , tags = Nothing
  , customThumbnail = Nothing
  , languageId = Nothing
  , nsfw = Nothing
  , honeypot = Nothing
  , altText = Nothing
  , body = Nothing
  , url = Nothing
  , communityId = cid
  , name = nm
  }


