{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.User where

import GHC.Generics

import Data.Time.Calendar (Day)

import Data.Aeson

import qualified Data.Text as T

-- internal imports

import Types.Auth

data User
  = User
    { userId        :: Int
    , userIdent     :: T.Text
    , userBalance   :: Int
    , userTimeStamp :: Day
    , userEmail     :: Maybe T.Text
    , userAvatar    :: Maybe Int
    , userSalt      :: AuthSalt
    , userHash      :: Maybe AuthHash
    , userAlgo      :: Maybe Int
    }
  | QueryUser
    { userId        :: Int
    , userIdent     :: T.Text
    , userAvatar    :: Maybe Int
    }
  deriving (Generic, Show)

instance ToJSON User where
  toEncoding (User id ident balance ts email avatar _ _ _) =
    pairs
      (  "userId" .= id
      <> "userIdent" .= ident
      <> "userBalance" .= balance
      <> "userTimeStamp" .= ts
      <> "userEmail" .= email
      <> "userAvatar" .= avatar
      )
  toEncoding (QueryUser id ident avatar) =
    pairs
      (  "userId" .= id
      <> "userIdent" .= ident
      <> "userAvatar" .= avatar
      )

instance FromJSON User

data UserSubmit = UserSubmit
  { userSubmitIdent :: T.Text
  , userSubmitEmail :: Maybe T.Text
  --, userSubmitPin   :: Maybe T.Text
  }
  deriving (Generic, Show)

instance ToJSON UserSubmit where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UserSubmit
