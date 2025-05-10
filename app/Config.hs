{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Config where

import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)
import Data.Text (Text, pack)

-- Main configuration structure
data Config = Config
    { lemmy :: LemmyConfig
    } deriving (Show, Generic)

-- Lemmy configuration
data LemmyConfig = LemmyConfig
    { instanceConfig :: [InstanceConfig]
    } deriving (Show, Generic)

-- Instance configuration
data InstanceConfig = InstanceConfig
    { lemmyInstance :: Instance
    } deriving (Show, Generic)

-- Instance details
data Instance = Instance
    { id :: Text
    , url :: Text
    , credentials :: Credentials
    } deriving (Show, Generic)

-- Credentials
data Credentials = Credentials
    { username :: Text
    , password_file :: Text
    , private_key_file :: Text
    } deriving (Show, Generic)

-- Automatically derive FromJSON instances
instance FromJSON Config
instance FromJSON LemmyConfig
instance FromJSON InstanceConfig
instance FromJSON Instance
instance FromJSON Credentials


-- | Load configuration from file
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> return $ Left (show err)
    Right config -> return $ Right config

-- Helper function to find an instance by ID
findInstance :: Text -> Config -> Maybe Instance
findInstance identifier config = 
    foldr (\cfg acc -> if cfg.lemmyInstance.id == identifier 
                       then Just (lemmyInstance cfg) 
                       else acc) 
          Nothing 
          (instanceConfig (lemmy config))

-- Get URL for an instance by ID
getInstanceUrl :: Text -> Config -> Either Text Text
getInstanceUrl identifier config =
    case findInstance identifier config of
        Just myInstance -> Right $ url myInstance
        Nothing -> Left $ pack "Instance not found: " <> identifier

-- Get username for an instance by ID
getInstanceUsername :: Text -> Config -> Either Text Text
getInstanceUsername identifier config =
    case findInstance identifier config of
        Just myInstance -> Right $ username (credentials myInstance)
        Nothing -> Left $ pack "Instance not found: " <> identifier

-- Get password file path for an instance by ID
getInstancePasswordFile :: Text -> Config -> Either Text Text
getInstancePasswordFile identifier config =
    case findInstance identifier config of
        Just myInstance -> Right $ password_file (credentials myInstance)
        Nothing -> Left $ pack "Instance not found: " <> identifier

-- Get private key file path for an instance by ID
getInstancePrivateKeyFile :: Text -> Config -> Either Text Text
getInstancePrivateKeyFile identifier config =
    case findInstance identifier config of
        Just myInstance -> Right $ private_key_file (credentials myInstance)
        Nothing -> Left $ pack "Instance not found: " <> identifier