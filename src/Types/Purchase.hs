{-# LANGUAGE DeriveGeneric #-}

module Types.Purchase where

import Data.Text

import Data.Aeson

import GHC.Generics


data PurchaseDetail = PurchaseDetail
  { pdBeverage :: Int
  , pdAmount   :: Int
  }
  deriving (Generic, Show)

instance ToJSON PurchaseDetail where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PurchaseDetail


data PurchaseResult = PurchaseResult
  { prFlag         :: PurchaseResultFlag
  , prMissingItems :: [PurchaseDetail]
  }
  deriving (Generic, Show)

instance ToJSON PurchaseResult where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PurchaseResult


data PurchaseResultFlag
  = PurchaseOK
  | PurchaseDebtful
  | PayAmount Int
  deriving (Generic, Show)

instance ToJSON PurchaseResultFlag where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PurchaseResultFlag
