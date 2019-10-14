{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Types.Journal where

import GHC.Generics

import qualified Data.Text as T (Text)

import Data.Aeson

import Data.Time (UTCTime)

data JournalEntry = JournalEntry
  { journalEntryId          :: Int
  , journalEntryDescription :: T.Text
  , journalEntryTimestamp   :: UTCTime
  , journalEntryAmount      :: Int
  , journalEntryTotalAmount :: Int
  , journalEntryIsCheck     :: Bool
  }
  deriving (Generic, Show)

instance ToJSON JournalEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalEntry


data JournalSubmit = JournalSubmit
  { journalSubmitDescription :: T.Text
  , journalSubmitAmount      :: Int
  }
  deriving (Generic, Show)

instance ToJSON JournalSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalSubmit


newtype JournalCashCheck = JournalCashCheck
  { journalCashCheckTotalAmount :: Int
  }
  deriving (Generic, Show)

instance ToJSON JournalCashCheck where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalCashCheck
