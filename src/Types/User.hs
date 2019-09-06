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
  deriving (Generic, Show)

instance ToJSON User where
  toEncoding (User id ident balance ts email avatar _ _ _) =
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


data UserDetails = UserDetails
  { userDetailsIdent   :: T.Text
  , userDetailsBalance :: Int
  , userDetailsEmail   :: Maybe T.Text
  , userDetailsAvatar  :: Maybe Int
  , userDetailsSalt    :: AuthSalt
  , userDetailsAlgo    :: Maybe AuthAlgorithm
  }
  deriving (Generic, Show)

instance ToJSON UserDetails where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserDetails


data UserDetailsSubmit = UserDetailsSubmit
  { userDetailsSubmitIdent   :: T.Text
  , userDetailsSubmitEmail   :: Maybe T.Text
  , userDetailsSubmitAvatar  :: Maybe Int
  , userDetailsSubmitHash    :: Maybe AuthHash
  , userDetailsSubmitAlgo    :: Maybe AuthAlgorithm
  }
  deriving (Generic, Show)

instance ToJSON UserDetailsSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserDetailsSubmit


data UserRecharge = UserRecharge
  { userRechargeAmount :: Int
  }
  deriving (Generic, Show)

instance ToJSON UserRecharge where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserRecharge


data UserTransfer = UserTransfer
  { userTransferTarget :: Int
  , userTransferAmount :: Int
  }
  deriving (Generic, Show)

instance ToJSON UserTransfer where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserTransfer
