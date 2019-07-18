{-# LANGUAGE DeriveGeneric #-}

module Types.Purchase where

import Data.Text

import GHC.Generics


data PurchaseDetail = PurchaseDetail
  { pdBeverage :: Int
  , pdAmount   :: Int
  }

instance ToJSON PurchaseDetail where
  toEncoding = genericToEncoding defaultOptions

instance fromJSON PurchaseDetail


data PurchaseResult
  = PurchaseOK
  | PurchaseDebtful
  | PayAmount Int
  deriving (Generic, Show)

instance ToJSON PurchaseResult where
  toEncoding = genericToEncoding defaultOptions

instance fromJSON PurchaseResult
