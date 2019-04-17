{-# LANGUAGE DeriveGeneric #-}
module Types.Token where

import GHC.Generics

import Data.Aeson

import Data.ByteString

import Data.Time.Clock (UTCTime)

data Token = Token
  { tokenString :: ByteString
  , tokenUser   :: Int
  , tokenExpiry :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON Token where
  toJSON (Token s _ _) = toJSON s
