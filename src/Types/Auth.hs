{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Auth where

import GHC.Generics

import Data.Aeson

import Data.ByteString

data AuthInfo = AuthInfo
  { authSalt :: AuthSalt
  , authAlgorithm :: AuthAlgorithm
  }
  deriving (Show, Generic)

instance ToJSON AuthInfo where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthInfo

data AuthAlgorithm
  = PBKDF2
    deriving (Show, Generic)

instance ToJSON AuthAlgorithm where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthAlgorithm

type AuthSalt = ByteString

instance ToJSON AuthSalt where
    toJSON salt = object (toHex salt)

instance FromJSON AuthSalt

data AuthRequest = AuthRequest
  { requestUser :: Int
  , requestHash :: ByteString
  }
  deriving (Show, Generic)

instance ToJSON AuthRequest where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthRequest

data AuthResult
  = Granted
    { authToken :: ByteString
    }
  | Denied
    deriving (Show, Generic)

instance ToJSON AuthResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthResult
