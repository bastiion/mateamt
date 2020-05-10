{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Types.Journal where

import GHC.Generics

import Data.Aeson

import Data.Time (UTCTime)

data JournalEntry = JournalEntry
  { journalEntryId          :: Int
  , journalEntryTimestamp   :: UTCTime
  , journalEntryUser        :: Maybe Int
  , journalEntryAction      :: JournalAction
  , journalEntryAmount      :: Int
  , journalEntryTotalAmount :: Int
  -- , journalEntryIsCheck     :: Bool
  }
  deriving (Generic, Show)

instance ToJSON JournalEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalEntry


data JournalSubmit = JournalSubmit
  { journalSubmitUser   :: Maybe Int
  , journalSubmitAction :: JournalAction
  , journalSubmitAmount :: Int
  }
  deriving (Generic, Show)

instance ToJSON JournalSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalSubmit


data JournalCashCheck = JournalCashCheck
  { journalCashCheckUser        :: Int
  , journalCashCheckTotalAmount :: Int
  }
  deriving (Generic, Show)

instance ToJSON JournalCashCheck where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalCashCheck

data JournalAction
  = CashCheck
  | Recharge
  | BuyCash
  | PayBill
  | PayOut
  deriving (Generic, Show, Enum, Eq)

instance ToJSON JournalAction where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalAction
