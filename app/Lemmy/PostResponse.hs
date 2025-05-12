{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lemmy.PostResponse where

import Data.Aeson (FromJSON, ToJSON, camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data PostResponse = PostResponse
  { postView :: Maybe PostView
  }
  deriving (Show, Generic)

instance FromJSON PostResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON PostResponse

data PostView = PostView
  { creatorBanned :: Maybe Bool,
    canMod :: Maybe Bool,
    tags :: Maybe [Tag],
    creatorIsAdmin :: Maybe Bool,
    creatorCommunityActions :: Maybe CreatorCommunityActions,
    creatorLocalInstanceActions :: Maybe CreatorInstanceActions,
    creatorHomeInstanceActions :: Maybe CreatorInstanceActions,
    instanceActions :: Maybe CreatorInstanceActions,
    postActions :: Maybe PostActions,
    personActions :: Maybe PersonActions,
    communityActions :: Maybe CreatorCommunityActions,
    imageDetails :: Maybe ImageDetails,
    community :: Maybe Community,
    creator :: Maybe Creator,
    post :: Maybe Post
  }
  deriving (Show, Generic)

instance FromJSON PostView where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON PostView

data Tag = Tag
  { deleted :: Maybe Bool,
    updated :: Maybe Text,
    published :: Maybe Text,
    communityId :: Maybe Double,
    displayName :: Maybe Text,
    apId :: Maybe Text,
    id :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON Tag where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON Tag

data CreatorCommunityActions = CreatorCommunityActions
  { banExpires :: Maybe Text,
    receivedBan :: Maybe Text,
    becameModerator :: Maybe Text,
    blocked :: Maybe Text,
    followApproverId :: Maybe Double,
    followState :: Maybe Text, -- Could be made more type-safe with an enum
    followed :: Maybe Text,
    personId :: Maybe Double,
    communityId :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON CreatorCommunityActions where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON CreatorCommunityActions

data CreatorInstanceActions = CreatorInstanceActions
  { banExpires :: Maybe Text,
    receivedBan :: Maybe Text,
    blocked :: Maybe Text,
    instanceId :: Maybe Double,
    personId :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON CreatorInstanceActions where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON CreatorInstanceActions

data PostActions = PostActions
  { hidden :: Maybe Text,
    likeScore :: Maybe Double,
    liked :: Maybe Text,
    saved :: Maybe Text,
    readCommentsAmount :: Maybe Double,
    readComments :: Maybe Text,
    read :: Maybe Text,
    personId :: Maybe Double,
    postId :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON PostActions where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON PostActions

data PersonActions = PersonActions
  { blocked :: Maybe Text,
    personId :: Maybe Double,
    targetId :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON PersonActions

instance ToJSON PersonActions

data ImageDetails = ImageDetails
  { blurhash :: Maybe Text,
    contentType :: Maybe Text,
    height :: Maybe Double,
    width :: Maybe Double,
    link :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON ImageDetails where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON ImageDetails

data Community = Community
  { localRemoved :: Maybe Bool,
    unresolvedReportCount :: Maybe Double,
    reportCount :: Maybe Double,
    subscribersLocal :: Maybe Double,
    usersActiveHalfYear :: Maybe Double,
    usersActiveMonth :: Maybe Double,
    usersActiveWeek :: Maybe Double,
    usersActiveDay :: Maybe Double,
    comments :: Maybe Double,
    posts :: Maybe Double,
    subscribers :: Maybe Double,
    description :: Maybe Text,
    visibility :: Maybe Text, -- Could be an enum
    instanceId :: Maybe Double,
    postingRestrictedToMods :: Maybe Bool,
    banner :: Maybe Text,
    icon :: Maybe Text,
    local :: Maybe Bool,
    apId :: Maybe Text,
    nsfw :: Maybe Bool,
    deleted :: Maybe Bool,
    updated :: Maybe Text,
    published :: Maybe Text,
    removed :: Maybe Bool,
    sidebar :: Maybe Text,
    title :: Maybe Text,
    name :: Maybe Text,
    id :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON Community where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON Community

data Creator = Creator
  { commentCount :: Maybe Double,
    postCount :: Maybe Double,
    instanceId :: Maybe Double,
    botAccount :: Maybe Bool,
    matrixUserId :: Maybe Text,
    deleted :: Maybe Bool,
    banner :: Maybe Text,
    local :: Maybe Bool,
    bio :: Maybe Text,
    apId :: Maybe Text,
    updated :: Maybe Text,
    published :: Maybe Text,
    avatar :: Maybe Text,
    displayName :: Maybe Text,
    name :: Maybe Text,
    id :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON Creator where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON Creator

data Post = Post
  { federationPending :: Maybe Bool,
    unresolvedReportCount :: Maybe Double,
    reportCount :: Maybe Double,
    newestCommentTime :: Maybe Text,
    downvotes :: Maybe Double,
    upvotes :: Maybe Double,
    score :: Maybe Double,
    comments :: Maybe Double,
    scheduledPublishTime :: Maybe Text,
    altText :: Maybe Text,
    urlContentType :: Maybe Text,
    featuredLocal :: Maybe Bool,
    featuredCommunity :: Maybe Bool,
    languageId :: Maybe Double,
    embedVideoUrl :: Maybe Text,
    local :: Maybe Bool,
    apId :: Maybe Text,
    thumbnailUrl :: Maybe Text,
    embedDescription :: Maybe Text,
    embedTitle :: Maybe Text,
    nsfw :: Maybe Bool,
    deleted :: Maybe Bool,
    updated :: Maybe Text,
    published :: Maybe Text,
    locked :: Maybe Bool,
    removed :: Maybe Bool,
    communityId :: Maybe Double,
    creatorId :: Maybe Double,
    body :: Maybe Text,
    url :: Maybe Text,
    name :: Maybe Text,
    id :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON Post where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }

instance ToJSON Post where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
        }
