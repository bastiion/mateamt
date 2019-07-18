{-# LANGUAGE DeriveGeneric #-}
module Types.Beverage where

import GHC.Generics

import Data.Aeson

import qualified Data.Text as T

data Beverage = Beverage
  { beverageId             :: Int
  , beverageIdent          :: T.Text
  , beveragePrice          :: Int
  , beverageAmount         :: Int
  , beverageVanish         :: Int
  , beverageMl             :: Int
  , beverageAvatar         :: Maybe Int
  , beverageSupplier       :: Maybe Int
  , beverageMaxAmount      :: Int
  , beverageTotalBought    :: Int
  , beverageAmountPerCrate :: Int
  , beveragePricePerCrate  :: Maybe Int
  , beverageArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON Beverage where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Beverage


data BeverageSubmit = BeverageSubmit
  { beverageSubmitIdent          :: T.Text
  , beverageSubmitPrice          :: Int
  -- , beverageSubmitAmount         :: Int
  -- , beverageSubmitVanish         :: Int
  , beverageSubmitMl             :: Int
  , beverageSubmitAvatar         :: Maybe Int
  , beverageSubmitSupplier       :: Maybe Int
  , beverageSubmitMaxAmount      :: Int
  -- , beverageSubmitTotalBought    :: Int
  , beverageSubmitAmountPerCrate :: Int
  , beverageSubmitPricePerCrate  :: Maybe Int
  , beverageSubmitArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON BeverageSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON BeverageSubmit
