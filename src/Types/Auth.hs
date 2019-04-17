{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Auth where

import GHC.Generics

import Data.Aeson

import Data.ByteString
import qualified Data.ByteString.Base16 as B16

import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
    deriving (Show, Read, Generic, Enum)

instance ToJSON AuthAlgorithm where
  toJSON = toJSON . show

instance FromJSON AuthAlgorithm where
  parseJSON j = read <$> parseJSON j

newtype AuthSalt = AuthSalt ByteString deriving (Show)

instance ToJSON AuthSalt where
  toJSON (AuthSalt bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthSalt where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthSalt enc)
      )

newtype AuthHash = AuthHash ByteString deriving (Show)

instance ToJSON AuthHash where
  toJSON (AuthHash bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthHash where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthHash enc)
      )

data AuthRequest = AuthRequest
  { requestUser :: Int
  , requestHash :: AuthHash
  }
  deriving (Show, Generic)

instance ToJSON AuthRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthRequest

data AuthResult
  = Granted
    { authToken :: AuthToken
    }
  | Denied
    deriving (Show, Generic)

instance ToJSON AuthResult where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthResult

newtype AuthToken = AuthToken ByteString deriving (Show)

instance ToJSON AuthToken where
  toJSON (AuthToken bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthToken where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthToken enc)
      )
