{-# LANGUAGE DeriveGeneric #-}
module Types.Product where

import GHC.Generics

import Data.Aeson

import qualified Data.Text as T

data Product = Product
  { productId             :: Int
  , productIdent          :: T.Text
  , productPrice          :: Int
  , productAmount         :: Int
  , productVanish         :: Int
  , productMl             :: Int
  , productAvatar         :: Maybe Int
  , productSupplier       :: Maybe Int
  , productMaxAmount      :: Int
  , productTotalBought    :: Int
  , productAmountPerCrate :: Int
  , productPricePerCrate  :: Maybe Int
  , productArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON Product where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Product


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
