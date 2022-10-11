{-# LANGUAGE OverloadedStrings #-}

module LoadToml where

import           Toml
import           Data.Text (Text)
import           Data.Set (Set)

data Settings = Settings { settingsPort :: Port
                         , settingsSecret :: !Text
                         , settingsClaims :: Set Text
                         }
  deriving Show

newtype Port = Port Int
  deriving Show

settingsCodec :: TomlCodec Settings
settingsCodec = Settings
  <$> Toml.diwrap (Toml.int "server.port") .= settingsPort
  <*> Toml.text "server.secret" .= settingsSecret
  <*> Toml.arraySetOf Toml._Text "claims" .= settingsClaims