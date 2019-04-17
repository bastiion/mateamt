{-# LANGUAGE DeriveGeneric #-}
module Types.Beverage where

import GHC.Generics

import Data.Aeson

import qualified Data.Text as T

data Beverage = Beverage
  { beverageIdent          :: T.Text
  , beveragePrice          :: Int
  , beverageAmount         :: Int
  , beverageVanish         :: Int
  , beverageMl             :: Int
  , beverageAvatar         :: Maybe Word
  , beverageSupplier       :: Maybe Word
  , beverageMaxAmount      :: Int
  , beverageTotalBought    :: Int
  , beverageAmonutPerCrate :: Int
  , beveragePricePerCrate  :: Maybe Int
  , beverageArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON Beverage where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Beverage
