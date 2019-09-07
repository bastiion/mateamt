{-# LANGUAGE DeriveGeneric #-}
module Types.Avatar where

import qualified Data.Text as T

import Data.Aeson

import GHC.Generics

data Avatar = Avatar
  { avatarId   :: Int
  , avatarName :: T.Text
  , avatarHash :: T.Text
  , avatarData :: T.Text
  }
  deriving (Show, Generic)

instance ToJSON Avatar where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Avatar


data AvatarData = AvatarData
  { avatarDataName :: T.Text
  , avatarDataData :: T.Text
  }
  deriving (Show, Generic)

instance ToJSON AvatarData where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AvatarData
