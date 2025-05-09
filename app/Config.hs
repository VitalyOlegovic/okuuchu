{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)
import qualified Data.Text as T

-- Configuration data types
data LemmyConfig = LemmyConfig
  { credentials :: Credentials
  , instanceConfig :: InstanceConfig
  } deriving (Generic, Show)

data Credentials = Credentials
  { username :: T.Text
  -- password field is commented out as per the config file
  -- , password :: T.Text
  } deriving (Generic, Show)

data InstanceConfig = InstanceConfig
  { url :: T.Text
  -- api_version is commented out as per the config file
  -- , apiVersion :: T.Text
  } deriving (Generic, Show)

-- Top-level configuration structure
data Config = Config
  { lemmy :: LemmyConfig
  } deriving (Generic, Show)

-- Automatically derive FromJSON instances
instance FromJSON Config
instance FromJSON LemmyConfig
instance FromJSON Credentials
instance FromJSON InstanceConfig

-- | Load configuration from file
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> return $ Left (show err)
    Right config -> return $ Right config