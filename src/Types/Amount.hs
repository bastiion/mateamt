{-# LANGUAGE DeriveGeneric #-}
module Types.Amount where

import GHC.Generics

import Data.Aeson

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
