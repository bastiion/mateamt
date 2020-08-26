{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Types.Amount where

import GHC.Generics

import Data.Aeson

import Data.Time (UTCTime)

-- internal imports

import Classes

data AmountUpdate = AmountUpdate
  { amountUpdateProductId  :: Int
  , amountUpdateRealAmount :: Int
  }
  deriving (Show, Generic)

instance ToJSON AmountUpdate where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AmountUpdate


data AmountRefill = AmountRefill
  { amountRefillProductId     :: Int
  , amountRefillAmountSingles :: Int
  , amountRefillAmountCrates  :: Int
  }
  deriving (Show, Generic)

instance ToJSON AmountRefill where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AmountRefill


data Amount = Amount
  { amountProductId :: Int
  , amountTimestamp :: UTCTime
  , amountAmount    :: Int
  , amountPrice     :: Int
  , amountVerified  :: Bool
  }
  deriving (Show)

instance ToDatabase Amount where

  type InTuple Amount = (Int, UTCTime, Int, Int, Bool)

  toDatabase (Amount pid ts amount price ver) =
    (pid, ts, amount, price, ver)

instance FromDatabase Amount where

  type OutTuple Amount = (Int, UTCTime, Int, Int, Bool)

  fromDatabase (pid, ts, amount, price, ver) =
    Amount pid ts amount price ver
