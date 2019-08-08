{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Journal where

data JournalEntry
  = JournalEntry
    { journalEntryDescription :: String
    , journalEntryTimestamp   :: UTCTime
    , journalEntryAmount      :: Int
    , journalEntryIsCheck     :: Bool
    }
