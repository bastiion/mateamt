{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Types.Product where

import GHC.Generics

import Data.Aeson

import qualified Data.Text as T

-- internal imports

import Classes

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

instance ToDatabase Product where

  type InTuple Product =
    (Int, T.Text, Int, Maybe Int, Maybe Int, Int, Int, Maybe Int, Maybe T.Text)

  toDatabase (Product id_ ident ml maid msid maxa apc ppc artnr) =
    (id_, ident, ml, maid, msid, maxa, apc, ppc, artnr)

instance FromDatabase Product where

  type OutTuple Product =
    (Int, T.Text, Int, Maybe Int, Maybe Int, Int, Int, Maybe Int, Maybe T.Text)

  fromDatabase (id_, ident, ml, maid, msid, maxa, apc, ppc, artnr) =
    Product id_ ident ml maid msid maxa apc ppc artnr


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


data ProductShortOverview = ProductShortOverview
  { productShortOverviewId             :: Int
  , productShortOverviewIdent          :: T.Text
  , productShortOverviewPrice          :: Int
  , productShortOverviewAmount         :: Int
  -- , productShortOverviewVanish         :: Int
  , productShortOverviewMl             :: Int
  , productShortOverviewAvatar         :: Maybe Int
  -- , productShortOverviewSupplier       :: Maybe Int
  -- , productShortOverviewMaxAmount      :: Int
  -- , productShortOverviewTotalBought    :: Int
  -- , productShortOverviewAmountPerCrate :: Int
  -- , productShortOverviewPricePerCrate  :: Maybe Int
  -- , productShortOverviewArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ProductShortOverview where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ProductShortOverview


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
