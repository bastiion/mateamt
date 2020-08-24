{-# LANGUAGE DeriveGeneric #-}
module Types.Meta where

import qualified Data.Text as T

import Data.Aeson

import GHC.Generics

data MetaInformation = MetaInformation
  { metaInfoVersion  :: T.Text
  , metaInfoCurrency :: T.Text
  , metaInfoDecimals :: Int
  }
  deriving (Show, Generic)

instance ToJSON MetaInformation where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MetaInformation
