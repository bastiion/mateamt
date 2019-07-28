{-# LANGUAGE DeriveGeneric #-}
module Types.Product where

import GHC.Generics

import Data.Aeson

import qualified Data.Text as T

data Product = Product
  { productId             :: Int
  , productIdent          :: T.Text
  -- , productPrice          :: Int
  -- , productAmount         :: Int
  -- , productVanish         :: Int
  , productMl             :: Int
  , productAvatar         :: Maybe Int
  , productSupplier       :: Maybe Int
  , productMaxAmount      :: Int
  -- , productTotalBought    :: Int
  , productAmountPerCrate :: Int
  , productPricePerCrate  :: Maybe Int
  , productArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON Product where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Product


data ProductOverview = ProductOverview
  { productOverviewId             :: Int
  , productOverviewIdent          :: T.Text
  , productOverviewPrice          :: Int
  , productOverviewAmount         :: Int
  , productOverviewVanish         :: Int
  , productOverviewMl             :: Int
  , productOverviewAvatar         :: Maybe Int
  , productOverviewSupplier       :: Maybe Int
  , productOverviewMaxAmount      :: Int
  , productOverviewTotalBought    :: Int
  , productOverviewAmountPerCrate :: Int
  , productOverviewPricePerCrate  :: Maybe Int
  , productOverviewArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ProductOverview where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ProductOverview


data ProductSubmit = ProductSubmit
  { productSubmitIdent          :: T.Text
  , productSubmitPrice          :: Int
  -- , productSubmitAmount         :: Int
  -- , productSubmitVanish         :: Int
  , productSubmitMl             :: Int
  , productSubmitAvatar         :: Maybe Int
  , productSubmitSupplier       :: Maybe Int
  , productSubmitMaxAmount      :: Int
  -- , productSubmitTotalBought    :: Int
  , productSubmitAmountPerCrate :: Int
  , productSubmitPricePerCrate  :: Maybe Int
  , productSubmitArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ProductSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ProductSubmit
