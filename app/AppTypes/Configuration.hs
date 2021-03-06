{-# LANGUAGE OverloadedStrings #-}
module AppTypes.Configuration where

import qualified Data.Text as T

import Data.YAML as Y

import Options.Applicative as O

data ServerConfig = ServerConfig
  { configDbHost :: T.Text
  , configDbPort :: Word
  , configDbName :: T.Text
  , configDbUser :: T.Text
  , configDbPasswd :: T.Text
  , configCurrencySymbol :: T.Text
  , configListenPort :: Word
  , configListenHost :: T.Text
  -- , configMaxConnectionsPerClient :: Word
  , configBlockRegistration :: Bool
  }
  deriving (Show)

instance FromYAML ServerConfig where
  parseYAML = withMap "Configuration" $ \m -> ServerConfig
    <$> m .: "db_host"
    <*> m .: "db_port"
    <*> m .: "db_name"
    <*> m .: "db_user"
    <*> m .: "db_passwd"
    <*> m .: "currency"
    <*> m .: "listen_port"
    <*> m .:? "listen_host" .!= "127.0.0.1"
    -- <*> m .:? "max_connections_per_client" .!= 10
    <*> m .: "block_registration"

data Options = Options
  { optConfigLocation :: T.Text
  , optMigrationLocation :: T.Text
  }
  deriving (Show)

options :: O.Parser Options
options = Options
  <$> strOption
    ( long "configuration"
    <> short 'c'
    <> metavar "FILEPATH"
    <> help "Location of the configuration YAML-file"
    )
  <*> strOption
    ( long "migration-dir"
    <> short 'm'
    <> metavar "DIRPATH"
    <> help "Location of the migration scripts folder"
    <> value "./migration_scripts"
    <> showDefault
    )
